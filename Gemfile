source 'https://rubygems.org'

git_source(:github) do |repo_name|
  repo_name = "#{repo_name}/#{repo_name}" unless repo_name.include?("/")
  "https://github.com/#{repo_name}.git"
end

gem 'rails', '~> 5.1.6'

gem 'pg', '~> 0.18'

gem 'puma', '~> 3.12'

gem 'bitters'
gem 'bourbon'
gem 'neat'
gem 'refills'
gem 'sass-rails', '~> 5.0'

gem 'webpacker'

gem 'jbuilder', '~> 2.5'

gem 'rspotify'

group :development, :test do
  gem 'dotenv-rails'
  gem 'pry-rails'
  gem 'rspec-rails'
end

group :development do
  gem 'web-console', '>= 3.3.0'
  gem 'listen', '>= 3.0.5', '< 3.2'
  gem 'spring'
  gem 'spring-watcher-listen', '~> 2.0.0'
end
