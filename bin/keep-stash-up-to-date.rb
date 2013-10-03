stash_project_names = Dir["/data/git/*/stash-project-name"]

stash_project_names.each do |thing|

  # get /data/git/(*) name (project name)
  thing =~ /.+\/([a-zA-Z0-9]+)\/stash-project-name/
  leverage_project = $1

  # get stash project name from file
  stash_project = ""
  file = File.open(thing, "r") do |infile|
    stash_project = infile.gets.chomp
  end

  # run the script to keep Stash up to date
  `/data/git/nwo/leverage-scripts/create-update-stash #{stash_project} #{leverage_project}`
end
