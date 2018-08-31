require 'net/http'
require 'rexml/document'
require 'date'
require 'fileutils'
include REXML


id = ARGV[0]
notes_dir = "#{Dir.home}/Documents/Notes"
path = "#{notes_dir}/#{id}.note"

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

  "<list><list-item dir='ltr'>" => "<li>",
  "</list-item></list>" => "</li>"

}

xml_body[0] = "<h1>#{title}</h1>"

match = false

xml_body.map do |xml_e|
  replacements.each do |k, v|
    if xml_e.include? k
      match = true
      xml_e.gsub!(k, v)
    end
    match = false
  end
    if match == false
      xml_e.prepend("<p>") # TODO, fix - this part is terrible html but displays well
      xml_e << "</p>"
    end
  end

xml_body.delete_at(-1)

File.open("#{id}.html", 'w') do |f|
  f.puts(xml_body) # writes a html file in the same dir as the script
end
