library(sodium)
# Create our initial hashed value
HASH_FILE <- "password.txt"
if (!file.exists(HASH_FILE)) {
  hashed <- sodium::password_store("AgeOld") # strong salted hash
  writeLines(hashed, HASH_FILE)
  try(Sys.chmod(HASH_FILE, "0600"), silent = TRUE) # best-effort restrictive perms
}