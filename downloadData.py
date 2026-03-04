from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
import time
path = "" #webdriver path
driver = webdriver.Chrome(service = Service(ChromeDriverManager().install()))
dates=[0,31,29,31,30,31,30,31,31,30,31,30,31]
for month in range(1,13):
        for date in range(1,dates[month]+1):
                if(month<10):
                    monthstr="0"+str(month)
                else:
                     monthstr=str(month)
                if(date<10):
                     datestr="0"+str(date)
                else:
                     datestr=str(date)
                url = "https://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=C0V760&stname=%25E6%25A9%258B%25E9%25A0%25AD&datepicker=2020-"+monthstr+"-"+datestr+"&altitude=30m";
                driver.get(url)
                time.sleep(1)
                download_button = driver.find_element(By.XPATH,'//*[@id="downloadCSV"]')
                download_button.click()
