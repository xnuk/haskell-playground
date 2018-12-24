require "http/client"

enum ResolverChannel
	LTS
	Nightly
end

def get_resolver(channel : ResolverChannel)
	resolver_channel =
		case channel
		when ResolverChannel::LTS
			"lts"
		when ResolverChannel::Nightly
			"nightly"
		end

	response = HTTP::Client.head "https://www.stackage.org/#{resolver_channel}"
	m = response.headers["location"].match %r{
		/(
			lts-[0-9\.]+
			|nightly-[0-9-]+
		)
	}x

	exit 1 if m.nil?

	m.captures[0]
end

def replace_resolver(path : String = "./stack.yaml")
	new_file = File.read_lines(path).map_with_index! do |line|
		return line unless line.starts_with? "resolver: "

		if line.starts_with? "resolver: lts"
			"resolver: #{get_resolver ResolverChannel::LTS}"
		elsif line.starts_with? "resolver: nightly"
			"resolver: #{get_resolver ResolverChannel::Nightly}"
		else
			line
		end
	end.join "\n"

	File.write path, new_file
	new_file
end

puts replace_resolver
