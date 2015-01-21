#!/usr/bin/python

import smtplib, socket, subprocess, os
from email.MIMEMultipart import MIMEMultipart
from email.MIMEText import MIMEText

hostname = socket.gethostname()
from_email = 'leancorreasoner@gmail.com'
from_addr = "Server %s %s" % (hostname, from_email)
to_addr = "leancor@adrianomelo.com"
subj = "Test finished on %s" % hostname
password = 'pass'

current_dir = os.path.abspath(os.path.dirname(__file__))
message = "Hello, we have some new results.\n\n"
message += subprocess.Popen("%s/report.sh" % current_dir, stdout=subprocess.PIPE, shell=True).stdout.read()
message += "\n\nCheers,\n%s" % hostname

msg = MIMEMultipart()
msg['From'] = from_addr
msg['To'] = to_addr
msg['Subject'] = subj
msg.attach(MIMEText(message))

mailserver = smtplib.SMTP('smtp.gmail.com',587)
# identify ourselves to smtp gmail client
mailserver.ehlo()
# secure our email with tls encryption
mailserver.starttls()
# re-identify ourselves as an encrypted connection
mailserver.ehlo()
mailserver.login(from_email, password)

mailserver.sendmail(from_addr,to_addr,msg.as_string())

mailserver.quit()