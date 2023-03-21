package com.polus.fibicomp.scheduler;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.codetable.dao.JSONParser;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.fastintegration.service.FastIntegrationService;
import com.polus.fibicomp.manpower.service.ManpowerService;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.report.service.ReportService;

@Component
@Transactional
public class Scheduler {

	protected static Logger logger = LogManager.getLogger(Scheduler.class.getName());


	@Autowired
	private HibernateTemplate hibernateTemplate;
	
	@Autowired
	public CommonDao commonDao;
	
	@Value("${person.temp.password}")
	private String personTempPassword;

	@Value("${person.feed.api}")
	private String personFeedAPI;
	
	@Value("${project.task.api}")
	private String projectTsakAPI;
	
	@Value("${project.expenditure.api}")
	private String projectExpenditureAPI;
	
	@Value("${person.feed.api.token}")
	private String personFeedAccessToken;
	
	@Value("${person.feed.api.token.type}")
	private String personFeedAccessTokenType;

	@Autowired
	public FastIntegrationService fastIntegrationService;

	@Value("${system.timezone}")
	private String timezone;

	@Autowired
	private ManpowerService manpowerService;
	
	@Autowired
	private ReportService reportService;

//	@Scheduled(cron = "${person.feed.api.schedule}",zone = Constants.CRON_JOB_TIMEZONE)
	public synchronized void processPersonFeed() throws Exception {
		if (commonDao.getParameterValueAsBoolean(Constants.PERSON_FEED_API_ENABLE)) {
		try {
			logger.info("person feed API execution");
			URL url = new URL(personFeedAPI);
			HttpURLConnection conn = (HttpURLConnection) url.openConnection();
			conn.setRequestMethod("GET");
			conn.setRequestProperty("Accept", "application/json");
			conn.setRequestProperty(personFeedAccessTokenType,  personFeedAccessToken);
			if (conn.getResponseCode() != 200) {
				throw new RuntimeException("Failed : HTTP error code : " + conn.getResponseCode());
			}
			BufferedReader br = new BufferedReader(new InputStreamReader((conn.getInputStream())));
			String output = br.readLine();
			JSONArray jsonObj = new JSONArray(output);
			List<Object> list = JSONParser.toList(jsonObj);
			String personId ="";
			for (int index = 0; index < list.size(); index++) {
				Person person = new Person();
				HashMap<String, Object> hmResult = (HashMap<String, Object>) list.get(index);
				person.setPersonId(hmResult.get("employeeNumber").toString());
				if (hmResult.get("assignmentCategory").toString().equalsIgnoreCase("staff")) {
					person.setIsServiceStaff(true);
				} 
				if (hmResult.get("assignmentCategory").toString().equalsIgnoreCase("faculty")) {
					person.setIsFaculty(true);
				}
				person.setDirectoryTitle(hmResult.get("assignmentCategory").toString());
				person.setPrincipalName(hmResult.get("samAccountName").toString());
				person.setFirstName(hmResult.get("firstName").toString());
				person.setLastName(hmResult.get("lastName").toString());
				person.setFullName(hmResult.get("fullName").toString());
				person.setHomeUnit(hmResult.get("organizationId").toString());
				person.setEmailAddress(hmResult.get("emailAddressKuUnified").toString());
				person.setUpdateTimestamp(new Timestamp(System.currentTimeMillis()));
				person.setUpdateUser("admin");
				if (hmResult.get("currentFlag").toString().equalsIgnoreCase("N")) {
					person.setStatus("I");
				} else {
					person.setStatus("A");
				}
				person.setPassword(personTempPassword);
				person.setSupervisorPersonId(hmResult.get("supervisorNumber").toString());
				if (personId != null && !personId .equalsIgnoreCase(person.getPersonId())) {	
					hibernateTemplate.saveOrUpdate(person);
				}	
				personId = person.getPersonId();
			}
			conn.disconnect();
		} catch (MalformedURLException e) {
			e.printStackTrace();
			logger.info("failed" + e);
		}catch (Exception e) {
			e.printStackTrace();
			logger.info("failed" + e);
		}
	}
	}	

	@Scheduled(cron = "${manpower.personntuexpiry.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public synchronized void manpowerClosingPosition() throws Exception {
		logger.info("Scheduler request for manpowerClosingPosition");
		manpowerService.manpowerClosingPosition();
	}
	
	@Scheduled(cron = "${manpower.view.base.salary.audit.report}", zone = Constants.CRON_JOB_TIMEZONE)
	public synchronized void roleRightAuditReport() {
		logger.info("Scheduler request for roleRightAuditReport");
		reportService.roleRightAuditReport();
	}

}
