require 'net/http'
require 'rexml/document'
require 'date'
require 'nextcloud'
require 'fileutils'
include REXML

=begin

"Will call external ruby script that returns a list of notes that are newer
than LocRev. Probably does something like
Runcommand('ruby', ['sync.rb', 'newnotes', inttostr(LocRev)]);
we then capture the output from RunCommand(), parse it and put result in
the NoteMeta list. Capture at least ID and RevNo and ideally LastChangeDate.
If all worked as expected, set result to true, if not, False and put something
into ErrorString."

https://github.com/tomboy-notes/tomboy-ng/wiki/Another-Sync-Model#implementation
https://github.com/tomboy-notes/tomboy-ng/blob/master/experimental/newsync/transnet.pas

RunCommand('ruby sync.rb get_new_notes 212')
RunCommand('ruby sync.rb upload 0B430A52-C425-4E7F-8A0C-E259468BD0AB')
RunCommand('ruby sync.rb download 0B430A52-C425-4E7F-8A0C-E259468BD0AB')
RunCommand('ruby sync.rb delete 0B430A52-C425-4E7F-8A0C-E259468BD0AB')

=end

NEXTCLOUD_USERNAME = ''
NEXTCLOUD_PASSWORD = ''
NEXTCLOUD_SERVER = ""
LOCAL_MANIFEST_PATH = "#{Dir.home}/.config/tomboy-ng/manifest.xml"
LOCAL_NOTES_PATH = "#{Dir.home}/Documents/Notes" # set this
NEXTCLOUD_NOTES_PATH = "Tomboy" # set this or ensure a root folder in Nextcloud called Tomboy

class RemoteServer

  def initialize #(*id, local_rev)

    @nextcloud = Nextcloud.webdav(
      url: NEXTCLOUD_SERVER,
      username: NEXTCLOUD_USERNAME,
      password: NEXTCLOUD_PASSWORD
    )

    parse_manifest

    # methods:

    # upload a note to remote: upload(id)
    # return note contents if it exists: download(id)
    # check if note exists: exists?(id)
    # delete from remote: delete(id)

    # return specified note's revision from remote manifest: extract_revision(id)
    # return specified note's last-change-date from remote manifest: extract_date(id)

  end

  def manifest
    @manifest # stores the text of the remote manifest
  end

  def note_data
    @note_data # stores the hash of note UUIDs and rev no
  end

  def download(id)
    # returns the text data of the file
    if exists?(id)
      @nextcloud.directory.download("#{NEXTCLOUD_NOTES_PATH}/#{id}.note")
    else
      "File #{id}.note not found in remote repository."
      # file not found! what do we do?
    end
  end

  def upload(id)
    @note = File.open("#{LOCAL_NOTES_PATH}/#{id}.note", "r")
    @nextcloud.directory.upload("#{NEXTCLOUD_NOTES_PATH}/#{id}.note", @note.read)
      # ^ if it doesn't find a note, make one
      # if it does find a note, update it and add a nextcloud version
    @note.close
  end

  def delete(id)
    @nextcloud.directory.destroy("#{NEXTCLOUD_NOTES_PATH}/#{id}.note")
    # todo: return a status instead of just acting
  end

  def exists?(id)
    @nextcloud.directory.find("#{NEXTCLOUD_NOTES_PATH}/#{id}.note").class != Hash
    # an error hash is returned if the file isn't found
  end

  def parse_manifest
    @manifest = @nextcloud.directory.download("#{NEXTCLOUD_NOTES_PATH}/manifest.xml")
    remote_manifest = File.open('rmanifest.xml','w+')
    note_lines = []

    File.open(remote_manifest, 'w') do |f|
      f.puts @manifest
    end

    remote_manifest.each_line do |l|
      if l.include?("<note ")
        note_lines.push(l) # after reading local manifest into a new file, scrape out the <note* lines
      end
    end

   note_lines.map! { |e| e.scan(/"(.*?(?<!\\))"/).flatten } # tidy up the note data
   @note_data = note_lines.to_h  # make a hash from the note data
   File.delete(remote_manifest) # remove temp file
  end

  def extract_date(id)
    text = download(id)[/<last-change-date>(.*?)<\/last-change-date>/]
    if text.nil? == false
      DateTime.parse(text)
    else
      "" # for notes that it doesn't have in its manifest, for now it returns "",
         # ... is bad, should fix.
    end
  end

  def extract_revision(id)
    @note_data.dig(id)
  end

  def get_new_notes(local_rev)
    new_notes = []
    @note_data.each do |note|
      if note[1].to_i > local_rev.to_i
        new_notes.push(["#{note[0]}", "#{note[1]}", extract_date("#{note[0]}")])
      end
    end
    new_notes # an array, as such: [[ID, rev, last-change-date], [ID, rev, ...], ...]]
  end
end

# 'logic' section

s = RemoteServer.new

case ARGV[0]
when "get_new_notes"
  # ruby sync.rb get_new_notes 212
  $stdout.write(s.get_new_notes(ARGV[1])) # ARGV[1] = local_rev
when "download"
  # ruby sync.rb download E325B331-9A96-4049-8BC1-84FB46F5FA8E
  $stdout.write(s.download(ARGV[1])) # ARGV[1] = note ID
when "upload"
  # ruby sync.rb upload E325B331-9A96-4049-8BC1-84FB46F5FA8E
  $stdout.write(s.upload(ARGV[1])) # = note ID
when "delete"
  # ruby sync.rb delete E325B331-9A96-4049-8BC1-84FB46F5FA8E
  $stdout.write(s.delete(ARGV[1])) # = note ID
else
  $stdout.write("Invalid argument to sync script.") # decide on an error code?
end
