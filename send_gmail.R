library(gmailr)
#https://developers.google.com/gmail/api/quickstart/python?authuser=1
# drees.zonage.ars@gmail.com
# gm_auth_configure(path = "credentials.json")
gm_auth(email = "drees.zonage.ars@gmail.com",cache = ".secrets/") # ça marche ! il fallait suivre les instructions de récup des credentials pour Python "quickstart.py"
#C:/Users/phile/.R/gargle/gargle-oauth
email <- gm_mime() %>%
  # gm_to("blandine.legendre@sante.gouv.fr") %>%
  gm_cc("phileas.condemine@sante.gouv.fr")%>%
  gm_subject("Envoi de mail via R") %>%
  gm_html_body(body = HTML("<p><b>Bonjour</b>,<br>",
                           "<i> Ceci est un test.</i><br>",
                           "A bientôt<br>",
                           "Philéas</p>"))

# gm_create_draft(email)

gm_send_message(email)
