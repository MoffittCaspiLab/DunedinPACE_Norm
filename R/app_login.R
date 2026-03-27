#-----------------------------------------------------
#-----------------------------------------------------
# App Login Screen
#-----------------------------------------------------
#-----------------------------------------------------

# Create our initial hashed value
  # Made with 'pwd_creation.R' which is in original repo

# Read Hashed PW back in
HASH_FILE <- "password.txt"
stored_hash <- trimws(readLines(HASH_FILE, warn = FALSE))
stopifnot(nzchar(stored_hash))

#Minimal in-memory credentials (change these)
credentials <- data.frame(
user = c("user1"),
#password = c("pass1"),
password=stored_hash,
admin = c(FALSE),
stringsAsFactors = FALSE
)

# Make a checker function for entered passwords
make_checker <- function(creds) {
  function(user, password) {
    ok_user <- isTRUE(user == creds$user[])
    ok_pwd <- FALSE
    # Use password_verify (handles salt/parameters embedded in the stored hash)
    if (ok_user) {
      ok_pwd <- tryCatch(
      sodium::password_verify(creds$password[], password),
      error = function(e) FALSE
      )
    }
    list(result = isTRUE(ok_user && ok_pwd), user = if (ok_user && ok_pwd) user else NULL)
    }
}