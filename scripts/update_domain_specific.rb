#!/usr/bin/ruby

require 'find'
require 'fileutils'
require 'timeout'
timeout = 5 # seconds

def puts(o) # puts is not atomic and messes up linebreaks with multiple threads
  print(o+"\n")
end
# colors
class String
  def colorize(color_code); "\e[#{color_code}m#{self}\e[0m" end
  def red; colorize(31) end
  def green; colorize(32) end
  def yellow; colorize(33) end
  def blue; colorize(34) end
  def pink; colorize(35) end
  def light_blue; colorize(36) end
end
# clear the current line
def clearline
  print "\r\e[K"
end

goblint = File.join(Dir.getwd,"goblint")
goblintbyte = File.join(Dir.getwd,"goblint.byte")
if File.exists?(goblintbyte) then
  puts "Running the byte-code version!"
  goblint = goblintbyte
elsif not File.exist?(goblint) then
  fail "Goblint not present in working directory. Please run script from goblint dir!"
end
vrsn = `#{goblint} --version`

testresults = File.expand_path("tests/domain_specific/results")
testfiles   = File.expand_path("tests/domain_specific")

alliswell = true
failed    = [] # failed tests
timedout  = [] # timed out tests

class Project
  attr_reader :name, :group, :path, :params, :warnings
  attr_writer :size
  def initialize(id, name, size, group, path, params, warnings)
    @id       = id
    @name     = name
    @size     = size
    @group    = group
    @path     = path
    @params   = params
    @warnings = warnings
  end
  def to_html
    orgfile = name + ".c.html"
    cilfile = name + ".cil.txt"
    "<td>#{@id}</td>\n" +
    "<td><a href=\"#{orgfile}\">#{@name}</a></td>\n" +
    "<td><a href=\"#{cilfile}\">#{@size} lines</a></td>\n"
  end
  def to_s
    "#{@name} (#{@url})"
  end
end

#Command line parameters
#Either only run a single test, or
#"future" will also run tests we normally skip
# -v at the end stands for verbose output
verbose = ARGV.last == "-v" && ARGV.pop
parallel = ARGV.last == "-p" && ARGV.pop
report = ARGV.last == "-r" && ARGV.pop
only = ARGV[0] unless ARGV[0].nil?
if only == "future" then
  future = true
  only = nil
elsif only == "group" then
  thegroup = ARGV[1]
  future = thegroup.start_with?"-"
  future = !future # why does negation above fail?
  only = nil
else
  future = false
end

# tracing = `grep 'tracing = true' src/config.ml`.size > 0
# if tracing then puts "Tracing in on!" else puts "Tracing is off" end

#processing the file information
projects = []
group = Dir.open(testfiles)
group.sort.each do |f|
  next if File.basename(f)[0] == ?.
  next if f =~ /goblin_temp/
  next unless f =~ /.*\.c$/
  id =  f[0..1]
  testname = f[3..-3]
  next unless only.nil? or testname == only
  path = File.expand_path(f, testfiles)
  lines = IO.readlines(path)
  size = 0
  debug = true

  next if not future and only.nil? and lines[0] =~ /SKIP/
  debug = false unless lines[0] =~ /DEBUG/
  lines[0] =~ /PARAM: (.*)$/
  if $1 then params = $1 else params = "" end

  hash = Hash.new
  i = 0
  lines.each do |obj|
    i = i + 1
    if obj =~ /#line ([0-9]+).*$/ then
      i = $1.to_i - 1
    end
    next if obj =~ /^\s*\/\//
    if obj =~ /RACE/ then
      hash[i] = if obj =~ /NORACE/ then "norace" else "race" end
    elsif obj =~ /DEADLOCK/ then
      hash[i] = if obj =~ /NODEADLOCK/ then "nodeadlock" else "deadlock" end
    elsif obj =~ /WARN/ then
      hash[i] = if obj =~ /NOWARN/ then "nowarn" else "warn" end
    elsif obj =~ /assert.*\(/ then
      debug = true
      if obj =~ /FAIL/ then
        hash[i] = "fail"
      elsif obj =~ /UNKNOWN/ then
        hash[i] = "unknown"
      else
        hash[i] = "assert"
      end
    end
  end
  case lines[0]
  when /NON?TERM/
    hash[-1] = "noterm"
    debug = true
  when /TERM/
    hash[-1] = "term"
    debug = true
  end
  params << " --set dbg.debug true" if debug
  p = Project.new(id,testname,size,"test",path,params,hash)
  projects << p
end

highlighter = lambda {|f,o| "cp #{f} #{o}"}
if report then
  cmds = {"code2html" => lambda {|f,o| "code2html -l c -n #{f} 2> /dev/null 1> #{o}"},
          "source-highlight" => lambda {|f,o| "source-highlight -n -i #{f} -o #{o}"},
          "pygmentize" => lambda {|f,o| "pygmentize -O full,linenos=1 -o #{o} #{f}"}
         }
  cmds.each do |name, cmd|
    # if `which #{cmd} 2> /dev/null`.empty? then
    if ENV['PATH'].split(':').map {|f| File.executable? "#{f}/#{name}"}.include?(true) then
      highlighter = cmd
      break
    end
  end
  if highlighter.nil? then
    puts "Warning: No syntax highlighter installed (code2html, source-highlight, pygmentize)."
  end
end

#analysing the files
startdir = Dir.pwd
doproject = lambda do |p|
  Dir.chdir(startdir)
  filepath = p.path
  dirname = File.dirname(filepath)
  filename = File.basename(filepath)
  Dir.chdir(dirname)
  clearline
  print "Testing #{p.group}/#{p.name}"
  warnfile = File.join(testresults, p.name + ".warn.txt")
  statsfile = File.join(testresults, p.name + ".stats.txt")
#   confile = File.join(testresults, p.name + ".con.txt")
#   solfile = File.join(testresults, p.name + ".sol.txt")
  cilfile = File.join(testresults, p.name + ".cil.txt")
  orgfile = File.join(testresults, p.name + ".c.html")
  if report then
    system(highlighter.call(filename, orgfile))
    `#{goblint} #{filename} --set justcil --enable kernel --sets ana.activated[+] thread true #{p.params} >#{cilfile} 2> /dev/null`
    p.size = `wc -l #{cilfile}`.split[0]
  end
  starttime = Time.now
  cmd = "#{goblint} #{filename} --enable kernel --sets ana.activated[+] thread  #{p.params} #{ENV['gobopt']} 1>#{warnfile} --set printstats true  2>#{statsfile}"
  pid = Process.spawn(cmd)
  begin
    Timeout::timeout(timeout) {Process.wait pid}
  rescue Timeout::Error
    puts "\t Timeout reached!".red + " Killing process #{pid}..."
    timedout.push "#{p.group}/#{p.name}"
    Process.kill('TERM', pid)
    return
  end
  endtime   = Time.now
  status = $?.exitstatus
  if status != 0 then
    reason = if status == 2 then "exception" elsif status == 3 then "verify" end
    puts "\t Status: #{status} (#{reason})".red
    stats = File.readlines statsfile
    if stats[0] =~ /exception/ then
      puts stats[0..9].join()
    end
    if status == 3 then
      warn = File.readlines warnfile
      puts (warn.select { |x| x["Unsatisfied constraint"] || x["Fixpoint not reached"] }).uniq.join()
    end
  end
#   `#{goblint} #{filename} #{p.params} --trace con 2>#{confile}` if tracing
#   `#{goblint} #{filename} #{p.params} --trace sol 2>#{solfile}` if tracing
  File.open(statsfile, "a") do |f|
    f.puts "\n=== APPENDED BY BENCHMARKING SCRIPT ==="
    f.puts "Analysis began: #{starttime}"
    f.puts "Analysis ended: #{endtime}"
    f.puts "Duration: #{format("%.02f", endtime-starttime)} s"
    f.puts "Goblint params: #{cmd}"
    f.puts vrsn
  end
end
if parallel then
  begin
    require 'parallel'
    Parallel.each projects, &doproject
  rescue LoadError => e
    puts "Missing dependency. Please run: sudo gem install parallel"
    raise e
  end
else
  projects.each &doproject
end
clearline
`pkill goblint` # FIXME somehow the killing above is not complete...

#Outputting
header = <<END
<head>
  <title>Tests (#{`uname -n`.chomp})</title>
  <style type="text/css">
    A:link {text-decoration: none}
    A:visited {text-decoration: none}
    A:active {text-decoration: none}
    A:hover {text-decoration: underline}
</style>
</head>
END
theresultfile = File.join(testresults, "index.html")
File.open(theresultfile, "w") do |f|
  f.puts "<html>"
  f.puts header
  f.puts "<body>"
  f.puts "<table border=2 cellpadding=4>"
  gname = ""
  projects.each do |p|
    is_ok = true
    if p.group != gname then
      gname = p.group
      headings = ["ID", "Name", "Size (CIL)", "Checks", "Time", "Problems"]
#       headings = ["ID", "Name", "Size (CIL)", "Checks", "Time", "Constraints", "Solver", "Problems"] if tracing
      f.puts "<tr><th colspan=#{headings.size}>#{gname}</th></tr>"
      f.puts "<tr>"
      headings.each {|h| f.puts "<th>#{h}</th>"}
      f.puts "</tr>"
    end
    f.puts "<tr>"
    f.puts p.to_html

    warnfile = p.name + ".warn.txt"
    warnings = Hash.new
    warnings[-1] = "term"
    lines = IO.readlines(File.join(testresults, warnfile))
    lines.each do |l|
      if l =~ /does not reach the end/ then warnings[-1] = "noterm" end
      next unless l =~ /(.*)\(.*\:(.*)\)/
      obj,i = $1,$2.to_i

      ranking = ["other", "warn", "race", "norace", "deadlock", "nodeadlock", "success", "fail", "unknown", "term", "noterm"]
      thiswarn =  case obj
                    when /lockset:/                  then "race"
                    when /Deadlock/                  then "deadlock"
                    when /Assertion .* will fail/    then "fail"
                    when /Assertion .* will succeed/ then "success"
                    when /Assertion .* is unknown/   then "unknown"
                    when /Uninitialized/             then "warn"
                    when /dereferencing of null/     then "warn"
                    when /CW:/                       then "warn"
                    when /Fixpoint not reached/      then "warn"
                    when /.*file handle.*/           then "warn"
                    when /.*file is never closed/    then "warn"
                    when /.*unclosed files: .*/      then "warn"
                    when /changed pointer .*/        then "warn"
                    else "other"
                  end
      oldwarn = warnings[i]
      if oldwarn.nil? then
        warnings[i] = thiswarn
      else
        warnings[i] = ranking[[ranking.index(thiswarn), ranking.index(oldwarn)].max]
      end
    end
    correct = 0
    ferr = nil
    p.warnings.each_pair do |idx, type|
      check = lambda {|cond|
        if cond then correct += 1
        else
          puts "Expected #{type}, but registered #{warnings[idx]} on #{p.name}:#{idx}" if verbose
          ferr = idx if ferr.nil? or idx < ferr
        end
      }
      case type
      when "deadlock", "race", "fail", "unknown", "noterm", "term", "warn"
        check.call warnings[idx] == type
      when "nowarn"
        check.call warnings[idx].nil?
      when "assert"
        check.call warnings[idx] == "success"
      when "norace"
        check.call warnings[idx] != "race"
      when "nodeadlock"
        check.call warnings[idx] != "deadlock"
      end
    end
    f.puts "<td><a href=\"#{warnfile}\">#{correct} of #{p.warnings.size}</a></td>"

    statsfile = p.name + ".stats.txt"
    lines = IO.readlines(File.join(testresults, statsfile))
    res = lines.grep(/^TOTAL\s*(.*) s.*$/) { $1 }
    errors = lines.grep(/Error:/)
    if res == [] or not errors == [] then
      is_ok = false
      f.puts "<td><a href=\"#{statsfile}\">failure</a></td>"
    else
      f.puts "<td><a href=\"#{statsfile}\">#{"%.2f" % res} s</a></td>"
    end

#     if tracing then
#       confile = p.name + ".con.txt"
#       lines = IO.readlines(File.join(testresults, confile))
#       cons = lines.grep(/con/).size
#       f.puts "<td><a href=\"#{confile}\">#{cons} nodes</a></td>"
#       solfile = p.name + ".sol.txt"
#       lines = IO.readlines(File.join(testresults, solfile))
#       sols = lines.grep(/sol: Entered/).size
#       f.puts "<td><a href=\"#{solfile}\">#{sols} nodes</a></td>"
#     end

    if correct == p.warnings.size && is_ok then
      f.puts "<td style =\"color: green\">NONE</td>"
    else
      alliswell = false
      if not timedout.include? "#{p.group}/#{p.name}" then
        failed.push p.name
        puts "#{p.group}/#{p.name} \e[31mfailed! \u2620\e[0m"
      end
      if not is_ok or ferr.nil? then
        f.puts "<td style =\"color: red\">FAILED</td>"
      else
        whataglorifiedmess = p.name + ".c.html"
        f.puts "<td><a href=\"#{whataglorifiedmess}#line#{ferr}\" style =\"color: red\">LINE #{ferr}</a></td>"
      end
    end

    f.puts "</tr>"
  end
  f.puts "</table>"
  f.print "<p style=\"font-size: 90%; white-space: pre-line\">"
  f.puts "Last updated: #{Time.now.strftime("%Y-%m-%d %H:%M:%S %z")}"
  f.puts "#{vrsn}"
  f.puts "</p>"
  f.puts "</body>"
  f.puts "</html>"
end

if report then
  puts "Usage examples for high-tech script parameters: "
  puts "  Single: ./scripts/update_suite.rb simple_rc"
  puts "  Groups: ./scripts/update_suite.rb group mutex"
  puts "  Exclude group: ./scripts/update_suite.rb group -mutex"
  puts "  Future: ./scripts/update_suite.rb future"
  puts "  Parallel execution: append -p"
  puts "  Verbose output: append -v"
  puts ("Results: " + theresultfile)
end
if alliswell then
  puts "All is well!".green
else
  puts "All is not well!".red
  # puts "failed tests: #{failed}"
end
exit alliswell
