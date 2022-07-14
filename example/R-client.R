library(openeo)

con = connect("http://127.0.0.1:8000")
login(user = "user",
      password = "password",
      login_type = "basic")

p = processes()
