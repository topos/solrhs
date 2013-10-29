namespace :run do
  desc "run main"
  task :run do
    Dir.chdir(SRC_DIR) do
      sh "./Main"
    end
  end
end


