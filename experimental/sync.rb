require 'net/http'
require 'rexml/document'
require 'date'
require 'nextcloud'
require 'fileutils'
include REXML

NOTE_ID = ARGV[0]

CONFIG_PATH = "#{Dir.home}/.config/tomboy-ng" # this + all below can be loaded in dynamically
CONFIG_FILE = "tomboy-ng.cfg"
LOCAL_MANIFEST_PATH = "#{Dir.home}/.config/tomboy-ng/manifest.xml"
NOTES_PATH = "#{Dir.home}/Documents/Notes" # definitely change this!

NEXTCLOUD_USERNAME = ''	 	# fill
NEXTCLOUD_PASSWORD = '' 	# these
NEXTCLOUD_SERVER = ''   	# out

class LocalNote

  def initialize(local_server)
    @local_server = local_server
    @id = local_server.id
    @file = local_server.find_note
    xml = Document.new(@file)
    @last_change_date = extract_date(xml, "last-change-date")
  end

  def id
    @id
  end

  def last_change_date
    @last_change_date
  end

  def extract_date(document, element)
    DateTime.parse(document.root.elements[element].text)
  end

  def file
    @file
  end

  def read
    File.open(@file, 'r') do |f|
      f.read
    end
  end

end

class LocalServer

  def initialize(id)
    @id = id
    @manifest = find_manifest
    @note = LocalNote.new(self)
    @remote = RemoteServer.new(self, @note)
  end

  def id
    @id
  end

  def note
    @note
  end

  def manifest
    @manifest
  end

  def remote
    @remote
  end

  def find_note
    File.open("#{NOTES_PATH}/#{@id}.note", 'r')
  end

  def find_manifest
    File.open(LOCAL_MANIFEST_PATH, 'r')
  end

  def delete
    File.delete("#{NOTES_PATH}/#{@id}.note")
  end

  def download
    File.open("#{NOTES_PATH}/#{@id}.note", 'w') do |file|
      File.open(@note.file, 'r') do |note|
        file.write(note.read)
      end
    end
  end

  def request_upload
    @remote.upload(@note)
  end

  def request_deletion
    @remote.delete(@note)
  end

  def marked_for_deletion
    lines = []
    doomed_notes = []
    xml = Document.new(@manifest)
    xml.root.elements["note-deletions"].children.each do |e|
      e = e.to_s
      lines.push(e)
    end
    lines.each do |line|
      if line[/........-....-....-....-............/]       # OH
      doomed_notes.push(line[/'(.*?[^\\])'/].gsub("'",""))  # MY
    end                                                     # WORD
    end
    doomed_notes.include?(@id)
    # doomed_notes is an array of all the IDs in <note-deletions>, local manifest
  end

end

class RemoteServer

  def initialize(local_server, note)

    @local_server = local_server
    @note = note

    @nextcloud = Nextcloud.webdav(
      url: NEXTCLOUD_SERVER,
      username: NEXTCLOUD_USERNAME,
      password: NEXTCLOUD_PASSWORD
    )

  end

  def upload(local_note)
    @nextcloud.directory.upload("Tomboy/#{local_note.id}.note", @note.read)
  end

  def delete(local_note)
    @nextcloud.directory.destroy("Tomboy/#{local_note.id}.note")
  end

  def exists?
    @nextcloud.directory.find("Tomboy/#{@note.id}.note").class != Hash
    # an error hash is returned if the file isn't found
  end

end

local = LocalServer.new(NOTE_ID)

# ^ the script starts by passing the note ID to a new local server obj.
# | the ARGV[0] would come from the .pas

################################### TESTS ######################################

# setup:
# $ gem install nextcloud

# usage:
# $ ruby sync.rb "0B430A52-C425-4E7F-8A0C-E259468BD0AB"

# from local, request the remote server deletes the note this module was passed

local.request_deletion
puts local.remote.exists? # => false

# from local, request the remote server receives & uploads the local note

local.request_upload
puts local.remote.exists? # => true, confirms upload works

# check again if the note can be deleted and checked...

local.request_deletion
puts local.remote.exists? # => false, confirms delete work
