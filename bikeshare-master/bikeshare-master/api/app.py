## This is an attempt at an API to access historical data.

from flask import Flask, make_response, request, jsonify
import flask
import os
import psycopg2
import datetime 

app = Flask(__name__)

conn = psycopg2.connect(dbname=os.environ.get('dbname'), user=os.environ.get('dbuser'), host=os.environ.get('dburl'))

def validator(city,year,month,day):
	city_list = {}

@app.route("/<city>/<int:year>/<int:month>/<int:day>")
def find_day(city,year,month,day):
	cur = conn.cursor()
	
	if (city=='bayarea'):
		cur.execute("SELECT * from bike_ind_bayarea WHERE timestamp::date='%s-%s-%s';", (year,month,day))
		results = cur.fetchall()
	elif (city=='chicago'):
		cur.execute("SELECT * from bike_ind_chicago WHERE timestamp::date='%s-%s-%s';", (year,month,day))
		results = cur.fetchall()
	else:
		return "bad query"
	print results
	out = {}
	for result in results:
		out[result[0]] = {}	
                for k,v in zip(['tfl_id','bikes','spaces','total_docks','timestamp'],result):
                	out[result[0]][k] = v
	resp = make_response(jsonify(out))
	resp.headers['Content-Type'] = 'application/json'
	return resp

if __name__ == "__main__":
    app.run(debug=True, port=8000, host='0.0.0.0')
