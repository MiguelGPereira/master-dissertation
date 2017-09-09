# -*- coding: utf-8 -*-
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import unittest, time, re

class SeleniumDataFetchTestPython(unittest.TestCase):
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://www.pordata.pt"
        self.verificationErrors = []
        self.accept_next_alert = True
    
    def test_selenium_data_fetch_test_python(self):
        driver = self.driver
        driver.get(self.base_url + "/")
        driver.find_element_by_id("wt11_wtMainContent_wt12_wtdivTitleRG").click()
        driver.find_element_by_xpath("//span[@id='wt9_wtMainContent_wt2_wtThemeRecordList']/div[13]/div/div/div[2]/h2").click()
        
        #driver.find_element_by_id("wt9_wtMainContent_wt2_wtThemeRecordList_ctl24_wt8_wtSubThemeRecordList_ctl04_wt5").click()
        wait = WebDriverWait(driver, 100)
        link = wait.until(EC.element_to_be_clickable((By.ID, "wt9_wtMainContent_wt2_wtThemeRecordList_ctl24_wt8_wtSubThemeRecordList_ctl04_wt5")))
        link.click()

        driver.find_element_by_id("wt8_wtMainContent_wtThemeBlockList_ctl26_wt30").click()
        driver.find_element_by_id("wt2_wtMainContent_wt5_wt189").click()
        driver.find_element_by_id("wt3_wtWorkspaceMode_wt1_wtbtnMoreData").click()
        driver.find_element_by_id("Site_Widgets_wt26_block_wtPopupContent_Site_Widgets_wt8_block_wtSearch_Theme_CB").click()
        Select(driver.find_element_by_id("Site_Widgets_wt26_block_wtPopupContent_Site_Widgets_wt8_block_wtSearch_Theme_CB")).select_by_visible_text(u"Justiça e Segurança")
        driver.find_element_by_id("Site_Widgets_wt26_block_wtPopupContent_Site_Widgets_wt8_block_wtSearch_SubTheme_CB").click()
        Select(driver.find_element_by_id("Site_Widgets_wt26_block_wtPopupContent_Site_Widgets_wt8_block_wtSearch_SubTheme_CB")).select_by_visible_text("Crimes")
        driver.find_element_by_id("Site_Widgets_wt26_block_wtPopupContent_Site_Widgets_wt8_block_wtSearch_Micro_CB").click()
        Select(driver.find_element_by_id("Site_Widgets_wt26_block_wtPopupContent_Site_Widgets_wt8_block_wtSearch_Micro_CB")).select_by_visible_text("Registados por categoria")
        driver.find_element_by_id("Site_Widgets_wt26_block_wtPopupContent_Site_Widgets_wt8_block_wt125").click()
        driver.find_element_by_id("wt3_wtWorkspaceMode_wt1_wt139").click()
    
    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException as e: return False
        return True
    
    def is_alert_present(self):
        try: self.driver.switch_to_alert()
        except NoAlertPresentException as e: return False
        return True
    
    def close_alert_and_get_its_text(self):
        try:
            alert = self.driver.switch_to_alert()
            alert_text = alert.text
            if self.accept_next_alert:
                alert.accept()
            else:
                alert.dismiss()
            return alert_text
        finally: self.accept_next_alert = True
    
    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

if __name__ == "__main__":
    unittest.main()
