require 'net/http'
require 'rexml/document'
require 'date'
require 'fileutils'
include REXML

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script accepts a note path as its only argument, and generates an HTML #
# version of the file in the same dir as the script.                          #
#                                                                             #
# $ ruby to_html.rb /path/to/notes/0B430A52-C425-4E7F-8A0C-E259468BD0AB.note  #
# => ./0B430A52-C425-4E7F-8A0C-E259468BD0AB.html                              #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

path = ARGV[0]
id = File.basename(path).gsub(".note", "")
p id
#notes_dir = "#{Dir.home}/Documents/Notes"
#path = "#{notes_dir}/#{id}.note"

html_body = []
xml_body = []

file = File.open(path, 'r')
xml = Document.new(file)

# EXTRACT

title = xml.root.elements["title"].text # grab the title

xml.elements.each("/note[@version=\"0.3\"]/text[@xml:space=\"preserve\"]") do |e|
  xml_body.push(e.to_s) # pull the body text out into an array
end

xml_body = xml_body[0].to_s.split("\n") # break lines into array elements

xml_body.map! {|e| e == "" ? e.gsub("", "\n") : e} # replace the ""s with linebreaks

# extract metadata in case we're keeping it?

create_date = xml.root.elements["/note[@version=\"0.3\"]/create-date/text()"]
last_change_date = xml.root.elements["/note[@version=\"0.3\"]/last-change-date/text()"]
last_metadata_change_date = xml.root.elements["/note[@version=\"0.3\"]/last-metadata-change-date/text()"]

# CONVERT

replacements = {

  "<bold>" => "<strong>",
  "</bold>" => "</strong>",

  "<italic>" => "<em>",
  "</italic>" => "</em>",

  "<underline>" => "<u>",
  "</underline>" => "</u>",

  "<strikeout>" => "<s>",
  "</strikeout>" => "</s>",

  "<highlight>" => "<span style=\"background-color:yellow\">",
  "</highlight>" => "</span>",

  "<monospace>" => "<code>",
  "</monospace>" => "</code>",

  "<size:small>" => "<small>",
  "</size:small>" => "</small>",

  "<size:large>" => "<h2>",
  "</size:large>" => "</h2>",

  "<size:huge>" => "<h1>",
  "</size:huge>" => "</h1>",

  # need to add one for retaining hyperlinks,
  # perhaps for linking to other notes that are exported in the same dir?

  "<list><list-item dir='ltr'>" => "<li>", # we also could do with a way to detect list start and end, adding in a <ul>
  "</list-item></list>" => "</li>"

}

xml_body[0] = "<h1>#{title}</h1>"

match = false

xml_body.map do |xml_e|
  replacements.each do |k, v|
    if xml_e.include? k
      match = true # we found a replaceable XML element -
      xml_e.gsub!(k, v) # - and so we sub it out with its HTML counterpart
    end
    match = false
  end
    if match == false
      xml_e.prepend("<p>") # TODO, fix - this part is terrible html but displays well
      xml_e << "</p>"
    end
  end

head = "<html xmlns:tomboy=\"http://beatniksoftware.com/tomboy\" xmlns:size=\"http://beatniksoftware.com/tomboy/size\" xmlns:link=\"http://beatniksoftware.com/tomboy/link\"><head><META http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"><title>#{title}</title></head><body>"

xml_body.unshift(head)
xml_body.delete_at(-1)
xml_body.push("</body></html>")

File.open("#{id}.html", 'w') do |f|
  f.puts(xml_body) # writes a html file in the same dir as the script
end
