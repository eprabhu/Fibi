package com.polus.fibicomp.personfeed;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TimeZone;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;
import javax.persistence.Query;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.json.JSONArray;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.claims.claimsIntegration.excelity.service.ExcelityService;
import com.polus.fibicomp.codetable.dao.JSONParser;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.DateTimeService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.pojo.Manpower;
import com.polus.fibicomp.manpowerintegration.dao.ManpowerIntegrationDao;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationLog;
import com.polus.fibicomp.person.controller.PersonController;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.roles.pojo.PersonRoles;

import oracle.jdbc.OracleTypes;

@Component
@Transactional
public class StudentsFeed {
	
	protected static Logger logger = LogManager.getLogger(StudentsFeed.class.getName());
	
	private ExecutorService executorService = Executors.newCachedThreadPool();
	
	@Autowired
	@Qualifier(value = "mailSender")
	public JavaMailSenderImpl mailSender;
	
	@Value("${spring.mail.username}")
	private String username;

	@Value("${spring.mail.password}")
	private String password;
	
	@Value("${spring.mail.properties.mail.smtp.auth}")
	private String auth;

	@Value("${spring.mail.properties.mail.smtp.starttls.enable}")
	private String tls;
	
	@Value("${spring.mail.host}")
	private String host;

	@Value("${spring.mail.port}")
	private String port;

	@Autowired
	private HibernateTemplate hibernateTemplate;
	
	@Value("${oracledb}")
	private String oracledb;
	
	@Autowired
	public CommonDao commonDao;
	
	@Value("${system.timezone}")
	private String timezone;
	
	@Value("${log.filepath}")
	private String filePath;
	
	@Value("${student.temp.password}")
	private String personTempPassword;

	@Value("${student.feed.api}")
	private String studentFeedAPI;
	
	@Value("${student.feed.api.token}")
	private String studentFeedAccessToken;
	
	@Value("${student.feed.api.token.type}")
	private String studentFeedAccessTokenType;
	
	@Value("${student.feed.retrycount}")
	private Integer retryCount;

	@Value("${person.feed.onoff}")
	private String personfeed_on_off;
	
	@Autowired
	private PersonController personController;
	
	Timestamp updateTimestamp;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private ManpowerIntegrationDao manpowerIntegrationDao;

	@Autowired
	DateTimeService dateTimeService;

	@Autowired
	private EmailService emailService;

	@Autowired
	private ExcelityService excelityService;

	@SuppressWarnings("unchecked")
	@Scheduled(cron = "${student.feed.api.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public synchronized void startStudentFeed() throws Exception {
		StringBuffer emailBody = new StringBuffer("");
		List<Person> nonValidPersons = new ArrayList<>();
		List<Person> nonValidPersonUnits = new ArrayList<>();
		List<Person> failedPersons = new ArrayList<>();
		List<Person> duplicateUserNames = new ArrayList<>();
		List<Person> invalidFacultys = new ArrayList<>();
		List<Person> invalidResearchStaffs = new ArrayList<>();
		List<Person> invalidSupportStaffs = new ArrayList<>();
		int count = 0;
		Integer responseCode = 0;
		if (commonDao.getParameterValueAsBoolean(Constants.STUDENT_FEED_API_ENABLE)) {
			if (personfeed_on_off.equalsIgnoreCase("on")) {
				try {
					logger.info("Student feed API execution starts at :" + commonDao.getCurrentTimestamp());
					emailBody.append("Student Feed API Summary :<br/>");
					logDetailsInFile("Student feed API call starts at");
					HttpURLConnection connection = null;
					do {
						try {
							logger.info("API calling count :" + count + "at : " + commonDao.getCurrentTimestamp());
							URL urls = new URL(studentFeedAPI);
							connection = (HttpURLConnection) urls.openConnection();
							connection.setRequestMethod("GET");
							connection.setRequestProperty("Accept", "application/json");
							connection.setRequestProperty(studentFeedAccessTokenType, studentFeedAccessToken);
							connection.setReadTimeout(60000);
							connection.setConnectTimeout(60000);
							connection.setDoInput(true);
							responseCode = connection.getResponseCode();
							if (responseCode != 200) {
								logDetailsInFile("API call failed with : HTTP error code : " + responseCode + " at");
								if (responseCode == 400) {// Bad request
									if (count == 0) {
										emailBody.append("<br/>API call failed due to " + "Bad request found");
									}
								} else if (responseCode == 401) {// Unauthorized
									if (count == 0) {
										emailBody.append("<br/>API call failed due to " + "Unauthorized Service");
									}
								} else if (responseCode == 404) {// Not found
									if (count == 0) {
										emailBody.append("<br/>API call failed due to " + "Service Not found");
									}
								} else if (responseCode == 408) {// Request Timeout
									if (count == 0) {
										emailBody.append("<br/>API call failed due to " + "Request Timeout");
									}
								} else if (responseCode == 500) {// internal server error
									if (count == 0) {
										emailBody.append("<br/>API call failed due to " + "Internal server error");
									}
								} else if (responseCode == 503) {// service unavailable
									if (count == 0) {
										emailBody.append("<br/>API call failed due to " + "Service unavailable");
									}
								} else if (responseCode == 403) {// Forbidden Issue
									if (count == 0) {
										emailBody.append("<br/>API call failed due to " + "Forbidden Issue");
									}
								}
								throw new RuntimeException("Failed : HTTP error code : " + responseCode);
							}
						} catch (Exception e) {
							count = count + 1;
							if (count < retryCount) {
								logger.info("delaying 2 minutes");
								Thread.sleep(120000);
							} else {
								emailBody.append("<br/> API call failure due to " + e);
							}
							// e.printStackTrace();
						}
					} while ((responseCode != 200) && (count < retryCount));

					BufferedReader bufferedReader = new BufferedReader(
							new InputStreamReader((connection.getInputStream())));
					String inputLine = "";
					StringBuilder response = new StringBuilder();
					while ((inputLine = bufferedReader.readLine()) != null) {
						response.append(inputLine);
					}
					bufferedReader.close();
//				String output = br.readLine();
					String apiResponseData = response.toString();
					JSONArray jsonData = new JSONArray(apiResponseData);
					List<Object> persons = JSONParser.toList(jsonData);
					int successCount = 0;
					int totalCountofData = persons.size();
					logger.info("Total count of data: " + totalCountofData);
					logDetailsInFile("Total count of student records from API: " + totalCountofData + " at");
					emailBody.append("<br/> Total count of student records: " + totalCountofData);
					updateTimestamp = commonDao.getCurrentTimestamp();
					logDetailsInFile("Processing of students feed API records to Fibi starts at");
					logger.info("Processing of students feed API records to Fibi starts at: "
							+ commonDao.getCurrentTimestamp());
					for (int index = 0; index < totalCountofData; index++) {
						Person person = new Person();
						HashMap<String, Object> hmResult = (HashMap<String, Object>) persons.get(index);
						String personsId = hmResult.get("personid").toString();
						if (personsId != null && !personsId.equalsIgnoreCase("null") && !personsId.isEmpty()) {
							person.setPersonId(personsId);
						}
						String isResearchStaff = hmResult.get("isresearchstaff").toString();
						if (!isResearchStaff.equalsIgnoreCase("Y") && !isResearchStaff.equalsIgnoreCase("N")) {
							invalidResearchStaffs.add(person);
						}
						if (isResearchStaff != null && isResearchStaff.equalsIgnoreCase(Constants.YES)) {
							person.setIsResearchStaff(true);
						} else {
							person.setIsResearchStaff(false);
						}
						String faculty = hmResult.get("isfaculty").toString();
						if (!faculty.equalsIgnoreCase("Y") && !faculty.equalsIgnoreCase("N")) {
							invalidFacultys.add(person);
						}
						if (faculty != null && faculty.equalsIgnoreCase(Constants.YES)) {
							person.setIsFaculty(true);
						} else {
							person.setIsFaculty(false);
						}
						String studenttype = hmResult.get("studenttype").toString();
						if (studenttype != null && !studenttype.equalsIgnoreCase("null")) {
							person.setIsGraduateStudentStaff(true);
							person.setPrimaryTitle(studenttype);
						} else {
							person.setIsGraduateStudentStaff(false);
						}
						String isSupportstaff = hmResult.get("issupportstaff").toString();
						if (!isSupportstaff.equalsIgnoreCase("Y") && !isSupportstaff.equalsIgnoreCase("N")) {
							invalidSupportStaffs.add(person);
						}
						if (isSupportstaff != null && isSupportstaff.equalsIgnoreCase(Constants.YES)) {
							person.setIsSupportStaff(true);
						} else {
							person.setIsSupportStaff(false);
						}
						String fullName = hmResult.get("fullname").toString();
						if (fullName != null && !fullName.equalsIgnoreCase("null")) {
							person.setFullName(fullName);
						}
						String emailId = hmResult.get("emailaddress").toString();
						if (emailId != null && !emailId.equalsIgnoreCase("null")) {
							person.setEmailAddress(emailId);
						}
						person.setUpdateTimestamp(updateTimestamp);
						person.setUpdateUser("quickstart");
						person.setStatus("A");
						String userName = hmResult.get("username").toString();
						if (userName != null && !userName.isEmpty() && !userName.equalsIgnoreCase("null")) {
							String myPersonId = checkUserNameInPerson(userName);
							if (myPersonId == null || myPersonId.equalsIgnoreCase(person.getPersonId())) {
								person.setPrincipalName(userName);
							} else {
								duplicateUserNames.add(person);
								logDetailsInFile("Student record with user name = NULL for PersonId: "
										+ person.getPersonId() + " at");
							}
						}
						String gender = hmResult.get("gender").toString();
						if (gender != null && !gender.equalsIgnoreCase("null")) {
							person.setGender(gender);
						}
						person.setPassword(personTempPassword);
						String unitNumber = hmResult.get("homeunit").toString();
						if (unitNumber != null && !unitNumber.equalsIgnoreCase("null")) {
//						logger.info("In Time valid check Unit : "+unitNumber+" at : " + commonDao.getCurrentTimestamp());
							// logDetailsInFile("In Time valid check Unit : " + unitNumber + " at :");
							if (checkValidUnitOrNot(unitNumber)) {
								person.setHomeUnit(unitNumber);
							} else {
								if (person.getIsGraduateStudentStaff() == false) {
									nonValidPersonUnits.add(person);
									logDetailsInFile("Invalid Unit : " + unitNumber + ", for PersonId : "
											+ person.getPersonId() + " at");
								}
							}
							// logDetailsInFile("Out Time valid check Unit : " + unitNumber + " at :");
//						logger.info("Out Time valid check Unit : "+unitNumber+" at : " + commonDao.getCurrentTimestamp());
						} else {
							if (person.getIsGraduateStudentStaff() == false) {
								nonValidPersonUnits.add(person);
								logDetailsInFile("Null Unit for personID: " + person.getPersonId() + " at");
							}
						}
						try {
//							logger.info("checkPersonDataTruncation in time at : " + commonDao.getCurrentTimestamp());
							// logDetailsInFile("checkPersonDataTruncation in time at : " );
							if (checkPersonDataTruncation(person)) {
								// logDetailsInFile("checkPersonDataTruncation Out time at : ");
								// logger.info("In Time save person at : " + commonDao.getCurrentTimestamp());
								// logDetailsInFile("In Time save person at :");
								// hibernateTemplate.saveOrUpdate(person);
//								String responseResult = processPersonInsertion(person);	
								// insert student record to db
								String responseResult = personController.savePersonFromFeed(person);
								// logger.info("Out Time save person at : " + commonDao.getCurrentTimestamp());
								// logDetailsInFile("Out Time save person at :");
								if (responseResult != null && responseResult.equalsIgnoreCase("Success")) {
									assignRoles(person);
									successCount = successCount + 1;
								} else {
									failedPersons.add(person);
									logDetailsInFile("Insertion of student record failed for PersonId:"
											+ person.getPersonId() + " at");
								}
							} else {
								logDetailsInFile("Invalid student record found for person with personId :"
										+ person.getPersonId() + " at");
								nonValidPersons.add(person);
							}
//								logger.info("checkPersonDataTruncation out time at : " + commonDao.getCurrentTimestamp());
							saveOrUpdatePersonDetailsToManpower(person, hmResult);
						} catch (Exception e) {
							// emailBody.append("<br/> Exception occured while insertion of person with
							// PersonId: " + person.getPersonId()+ "<br/>" + e + "<br/>" + " at : " +
							// commonDao.getCurrentTimestamp());
							logDetailsInFile("Exception occured while insertion of student with PersonId: "
									+ person.getPersonId() + "<br/>" + e + "<br/>" + " at");
							e.printStackTrace();
						}
//					personId = person.getPersonId();
					}
					connection.disconnect();
					logDetailsInFile("Processing of student feed records to Fibi completed at");
					logger.info(
							"Processing of Student feed data to Fibi completed at: " + commonDao.getCurrentTimestamp());
					logDetailsInFile("Success Count : " + successCount + " at");
					emailBody.append("<br/>Success Count : " + successCount);
					emailBody.append("<br/>Failed Count :" + (totalCountofData - successCount));
					logDetailsInFile("Failed Count:" + (totalCountofData - successCount) + " at");
					// calling procedure to set persons inactive
					logger.info("Time in set Inactive students: " + commonDao.getCurrentTimestamp());
					logDetailsInFile("Process to set persons Inactive started at");
					Integer inactivePersonRecords = callDeactivatePersons();
					logger.info("inactivePersonRecords : {}",  inactivePersonRecords);
					logger.info("Time out set Inactive students: " + commonDao.getCurrentTimestamp());
					logDetailsInFile("Process to set persons Inactive ended at");
					// sync person Roles after assigning roles !!!
					logDetailsInFile("Sync PersonRole started at");
					refreshPersonRoleRT();
					logDetailsInFile("Sync PersonRole ended at");
					getAwardManpowerInactivePerson();
				} catch (MalformedURLException e) {
					e.printStackTrace();
					logger.info("failed" + e);
					emailBody.append("<br/> MalformedURLException : " + e);
					logDetailsInFile("MalformedURLException" + e + " at");
				} catch (Exception e) {
					e.printStackTrace();
//				emailBody.append("<br/> Exception occured : " + e);
					logDetailsInFile("Exception occured: " + e + " at");
					logger.info("failed" + e);
				}
				logger.info("Student feed API execution ends at :" + commonDao.getCurrentTimestamp());
				logDetailsInFile("Student feed API execution ends at");
//			logger.info("emailServiceCall in : " + commonDao.getCurrentTimestamp());
				logDetailsInFile("emailService Call at");
				emailServiceCall(emailBody.toString(), nonValidPersonUnits, nonValidPersons, failedPersons,
						duplicateUserNames, invalidFacultys, invalidResearchStaffs, invalidSupportStaffs);
				logDetailsInFile("STUDENTS RECORD FEED PROCESSES FINISHED AT");
			}
		}
	}

	private void getAwardManpowerInactivePerson() {
		try {
			List<Person> inactivePersons = personDao.getInactivePersons();
			if (!inactivePersons.isEmpty()) {
				Timestamp executionStartTime = inactivePersons.get(0).getUpdateTimestamp();
				Set<String> personIds = inactivePersons.stream().map(Person::getPersonId).collect(Collectors.toSet());
				if (!personIds.isEmpty()) {
					List<AwardManpowerResource> awardManpowerResources = new ArrayList<>();
					List<Object[]> entities = personDao.getInactivePersonsFromManpower(personIds, executionStartTime);
					for (Object[] entity : entities) {
						AwardManpowerResource awardManpowerResource = new AwardManpowerResource();
						if (entity[0] != null && entity[1] != null && entity[2] != null) {
							awardManpowerResource.setAwardId(Integer.parseInt(entity[0].toString()));
							awardManpowerResource.setPersonId(entity[1].toString());
							awardManpowerResource.setFullName(entity[2].toString());
							awardManpowerResources.add(awardManpowerResource);
						}
					}
					if (!awardManpowerResources.isEmpty()) {
						Map<Integer, List<AwardManpowerResource>> manpowerPersons = awardManpowerResources.stream().collect(Collectors.groupingBy(AwardManpowerResource::getAwardId));
						manpowerPersons.entrySet().stream().forEach(manpower -> {
							prepareInactiveanpowerPersonAndSendMail(manpower.getKey(), manpower.getValue());
						});
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
			logDetailsInFile("Exception occured in getAwardManpowerInactivePerson: " + e + " at");
			logger.info("failed" + e);
		}
	}

	private void prepareInactiveanpowerPersonAndSendMail(Integer awardId, List<AwardManpowerResource> manpowerPersons) {
		logger.info("Received request for inactive manpower person mail for sending");
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(Constants.MANPOWER_INACTIVE_PERSON_NOTIFICATION_CODE);
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
		emailServiceVO.setModuleItemKey(awardId.toString());
		emailServiceVO.setPlaceHolder(getManpowerPlaceholders(manpowerPersons));
		emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
		emailServiceVO.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		emailServiceVO = emailService.sendEmail(emailServiceVO);
	}

	private Map<String, String> getManpowerPlaceholders(List<AwardManpowerResource> manpowerPersons) {
		StringBuilder fullName = new StringBuilder();
		Map<String, String> placeHolder = new HashMap<>();
		manpowerPersons.stream().forEach(manpowerPerson -> {
			fullName.append(manpowerPerson.getFullName() != null ? manpowerPerson.getFullName() : "");
			fullName.append("<br/>");
		});
		placeHolder.put("{PERSON_NAME}", fullName.toString());
		return placeHolder;
	}

	private void saveOrUpdatePersonDetailsToManpower(Person person, HashMap<String, Object> hmResult) {
		try {
		Manpower manpowerDetail = new Manpower();
		if (person != null && person.getPersonId() != null) {
			manpowerDetail = personDao.getManpowerDetailsbyPersonId(person.getPersonId());
			String cadidatureStartDate = hmResult.get("candidaturE_START_DATE").toString();
			String cadidatureEndDate = hmResult.get("candidaturE_END_DATE").toString();
			String nationality = hmResult.get("nationality").toString();
			String citizenship = hmResult.get("citizenship").toString();
			if ((manpowerDetail.getManpowerPersonId() == null ||  manpowerDetail.getCandidatureStartDate() != null)
					&& cadidatureStartDate != null && !cadidatureStartDate.equalsIgnoreCase("null")) {
				Date varDate = new SimpleDateFormat("dd-MMM-yyyy").parse(cadidatureStartDate);
				manpowerDetail.setCandidatureStartDate(Timestamp.valueOf(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(varDate)));
			}
			if ((manpowerDetail.getManpowerPersonId() == null ||  manpowerDetail.getCandidatureEndDate() != null) && cadidatureEndDate != null
					&& !cadidatureEndDate.equalsIgnoreCase("null")) {
				Date varDate = new SimpleDateFormat("dd-MMM-yyyy").parse(cadidatureEndDate);
				manpowerDetail.setCandidatureEndDate(Timestamp.valueOf(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(varDate)));
			}
			if ((manpowerDetail.getManpowerPersonId() == null ||  manpowerDetail.getNationality() != null) && nationality != null
					&& !nationality.equalsIgnoreCase("null")) {
				manpowerDetail.setNationality(excelityService.encryptAES(nationality));
			}
			if ((manpowerDetail.getManpowerPersonId() == null ||  manpowerDetail.getCitizenship() != null) && citizenship != null && !citizenship.equalsIgnoreCase("null")) {
				manpowerDetail.setCitizenship(excelityService.encryptAES(citizenship));
			}
			manpowerDetail.setFullName(person.getFullName());
			manpowerDetail.setManpowerPersonId(person.getPersonId());
			manpowerDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			manpowerIntegrationDao.saveOrUpdate(manpowerDetail);
		}
		} catch(Exception e) {
			logger.info("error in saveOrUpdatePersonDetailsToManpower" + e.getMessage());
		}
	}

	private void assignRoles(Person person) {
		if (person.getHomeUnit() != null) {
			if (person.getIsFaculty() == true) {
//				logger.info("In Time assign Person Role for PersonId: " + person.getPersonId()+ " at : " + commonDao.getCurrentTimestamp());
				//logDetailsInFile("Assign PI role starts for PersonId : " + person.getPersonId()+ " at :");
				assignStudentRole(person, Constants.PI_RESEARCH_ASSISTANTS);
//				logger.info("Out Time assign Person Role for PersonId: " + person.getPersonId()+ " at : " + commonDao.getCurrentTimestamp());
				//logDetailsInFile("Assign PI role ends for PersonId : " + person.getPersonId()+ " at :");
			}
			if (person.getIsResearchStaff() == true) {
//				logger.info("In Time assign Create proposal Role for PersonId: " + person.getPersonId()+ " at : " + commonDao.getCurrentTimestamp());
				//logDetailsInFile("Assign create proposal role starts for PersonId : "+ person.getPersonId() + " at :");
				assignStudentRole(person, Constants.CREATE_PROPOSAL);
//				logger.info("Out Time assign Create proposal Role for PersonId: " + person.getPersonId()+ " at : " + commonDao.getCurrentTimestamp());
				//logDetailsInFile("Assign create proposal role ended for PersonId : "+ person.getPersonId() + " at :");
			}
		}	
	}

	private String checkUserNameInPerson(String principalName) {
		String personId = null;
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "select personId from Person where principalName =: principalName";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("principalName", principalName);
			personId = query.getSingleResult().toString();
		} catch (Exception e) {
			logger.info("NoResultException: No entity found for query");
		}
		return personId == null ? null : personId;
	}

	private void emailServiceCall(String emailBody, List<Person> nonValidPersonUnits, List<Person> nonValidPersons,
			List<Person> insertionFailedPersons, List<Person> duplicateUserNames, List<Person> invalidFacultys,
			List<Person> invalidResearchStaffs, List<Person> invalidSupportStaffs) {
		StringBuffer sbuffer = new StringBuffer();
		if (nonValidPersonUnits != null && !nonValidPersonUnits.isEmpty()) {
			sbuffer.append("<br/><br/>Student records with invalid & null units are listed below : <br/> PersonIds : ");
			sbuffer.append("[ ");
			for (Person person : nonValidPersonUnits) {
				sbuffer.append("" + person.getPersonId());
				sbuffer.append(",");
			}
			sbuffer.deleteCharAt(sbuffer.length() - 1);
			sbuffer.append(" ]");
		}
		if (nonValidPersons != null && !nonValidPersons.isEmpty()) {
			sbuffer.append("<br/><br/>Student records with invalid data are listed below : <br/> PersonIds : ");
			sbuffer.append("[ ");
			for (Person person : nonValidPersons) {
				sbuffer.append("" + person.getPersonId());
				sbuffer.append(",");
			}
			sbuffer.deleteCharAt(sbuffer.length() - 1);
			sbuffer.append(" ]");
		}
		if (insertionFailedPersons != null && !insertionFailedPersons.isEmpty()) {
			sbuffer.append("<br/><br/>Student records failed on insertion are listed below : <br/> PersonIds : ");
			sbuffer.append("[ ");
			for (Person person : insertionFailedPersons) {
				sbuffer.append("" + person.getPersonId());
				sbuffer.append(",");
			}
			sbuffer.deleteCharAt(sbuffer.length() - 1);
			sbuffer.append(" ]");
		}
		if (duplicateUserNames != null && !duplicateUserNames.isEmpty()) {
			sbuffer.append("<br/><br/>Student records with duplicate username are listed below : <br/> PersonIds : ");
			sbuffer.append("[ ");
			for (Person person : duplicateUserNames) {
				sbuffer.append("" + person.getPersonId());
				sbuffer.append(",");
			}
			sbuffer.deleteCharAt(sbuffer.length() - 1);
			sbuffer.append(" ]");
		}
		// invalid faculty
		if (invalidFacultys != null && !invalidFacultys.isEmpty()) {
			sbuffer.append("<br/><br/>Student records with invalid Faculty are listed below : <br/> PersonIds : ");
			sbuffer.append("[ ");
			for (Person person : invalidFacultys) {
				sbuffer.append("" + person.getPersonId());
				sbuffer.append(",");
			}
			sbuffer.deleteCharAt(sbuffer.length() - 1);
			sbuffer.append(" ]");
		}
		// invalid research staff
		if (invalidResearchStaffs != null && !invalidResearchStaffs.isEmpty()) {
			sbuffer.append("<br/><br/>Student records with invalid Research staff are listed below : <br/> PersonIds : ");
			sbuffer.append("[ ");
			for (Person person : invalidResearchStaffs) {
				sbuffer.append("" + person.getPersonId());
				sbuffer.append(",");
			}
			sbuffer.deleteCharAt(sbuffer.length() - 1);
			sbuffer.append(" ]");
		}
		// invalid support staff
		if (invalidSupportStaffs != null && !invalidSupportStaffs.isEmpty()) {
			sbuffer.append("<br/><br/>Student records with invalid Support staff are listed below : <br/> PersonIds : ");
			sbuffer.append("[ ");
			for (Person person : invalidSupportStaffs) {
				sbuffer.append("" + person.getPersonId());
				sbuffer.append(",");
			}
			sbuffer.deleteCharAt(sbuffer.length() - 1);
			sbuffer.append(" ]");
		}
		emailBody = emailBody + sbuffer.toString();
		sendEmail("Student Feed Report", emailBody);
	}
	
	private void refreshPersonRoleRT() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call REFRESH_PERSON_ROLE_RT ()}");
				statement.execute();
				statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "REFRESH_PERSON_ROLE_RT ";
				String functionCall = "{call " + procedureName + "(?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.execute();
				statement.getObject(1);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private Integer callDeactivatePersons() {
		Integer count = 0;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		try {
			if (oracledb.equalsIgnoreCase(Constants.dbMySQL)) {
				statement = connection.prepareCall("{call DELETE_INACTIVE_PERSONS(?,?)}");
				statement.setString(1, "STUDENT");
				statement.setTimestamp(2, updateTimestamp);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase(Constants.dbOracle)) {
				String procedureName = "DELETE_INACTIVE_PERSONS";
				String functionCall = "{call " + procedureName + "(?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, "STUDENT");
				statement.setTimestamp(3, updateTimestamp);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			if (resultSet != null) {
				while (resultSet.next()) {
					count = resultSet.getInt("DELETED_PERSON_COUNT");
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return count;
	}

	private Boolean checkPersonDataTruncation(Person person) {
		// checking all data sizes of a person
		if (person.getPersonId() != null) {
			if (!(person.getPersonId().length() <= 40)) {
				logDetailsInFile("Person Data Truncation occured for PersonId: " + person.getPersonId()+ " at");
				return false;
			}
		}
		if (!(person.getFullName().length() <= 90)) {
			logDetailsInFile("Person Data Truncation occured at Full Name: " + person.getFullName()+ " for PersonId: " + person.getPersonId()+ " at");
			return false;
		}
		if (person.getPrincipalName() != null) {
			if (!(person.getPrincipalName().length() <= 51)) {
				logDetailsInFile("Person Data Truncation occured at User Name: " + person.getPrincipalName()+ "for PersonId: " + person.getPersonId()+ " at");
				return false;
			}
		}
		if (person.getEmailAddress()!= null) {
		if (!(person.getEmailAddress().length() <= 60)) {
			logDetailsInFile("Person Data Truncation occured at email Address: "+ person.getEmailAddress() + "for PersonId: " + person.getPersonId()+ " at");
			return false;
		}
		}
		if (person.getHomeUnit() != null) {
			if (!(person.getHomeUnit().length() <= 8)) {
				logDetailsInFile("Person Data Truncation occured at Unit Number: "+ person.getHomeUnit() + " for PersonId: " + person.getPersonId()+ " at");
				return false;
			}
		}
		if (person.getPrimaryTitle() != null) {
			if (!(person.getPrimaryTitle().length() <= 51)) {
				logDetailsInFile("Person Data Truncation occured at PrimaryTitle :" + person.getPrimaryTitle()+ " for PersonId:" + person.getPersonId()+ " at");
				return false;
			}
		}
		return true;
	}

	private Boolean checkValidUnitOrNot(String homeUnit) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        String hqlQuery = "select count(*) from Unit where unitNumber =: unitNumber";
        Query query = session.createQuery(hqlQuery);
        query.setParameter("unitNumber", homeUnit);
        Integer count = Integer.parseInt(query.getSingleResult().toString());
        return count == 0 ? false : true;
	}	

	public void logDetailsInFile(String fileContent) {
		try {
			String fileName = "Student_Feed_Logs";
			String date = convertDateFormatBasedOnTimeZone(commonDao.getCurrentTimestamp().getTime(), Constants.LOG_DATE_FORMAT);
			DateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
			Date currentDate = new Date(commonDao.getCurrentTimestamp().getTime());
			dateFormat.setTimeZone(TimeZone.getTimeZone(timezone));
			String fileNameWithPath = filePath + File.separator + fileName + "_" + date + ".log";
			File file = new File(fileNameWithPath);
			FileOutputStream fileOutputStream = null;
			if (file.exists()) {
				fileOutputStream = new FileOutputStream(fileNameWithPath, true);
			} else {
				fileOutputStream = new FileOutputStream(fileNameWithPath);
			}
			fileContent = fileContent + " - " + dateFormat.format(currentDate) + "\n";
			fileOutputStream.write(fileContent.getBytes());
			fileOutputStream.close();
		} catch (Exception e) {
			logger.error("Exception in method logDetailsInFile : {} ", e.getMessage());
		}
	}
	public String convertDateFormatBasedOnTimeZone(Long dateValue,String dateFormat) {
		Date date = new Date(dateValue);
		String formattedDate = new SimpleDateFormat(dateFormat).format(commonDao.adjustTimezone(date));
		return formattedDate;
	}
	
	public void sendEmail(String subject, String body) {
		logger.info("Received request for mail sending");
		NotificationLog notificationLog = new NotificationLog();
		if (mailSender != null) {
			mailSender.setUsername(username);
			mailSender.setPassword(password);
			Properties props = mailSender.getJavaMailProperties();
			props.put("mail.smtp.auth", auth);
			props.put("mail.smtp.starttls.enable", tls);
			props.put("mail.smtp.port", port);
			props.put("mail.smtp.host", host);

			MimeMessage message = mailSender.createMimeMessage();
			MimeMessageHelper helper = null;
			notificationLog.setFromUserEmailId(username);
			notificationLog.setSubject("StudentFeedLog");
			notificationLog.setMessage(body);
			notificationLog.setSendDate(commonDao.getCurrentTimestamp());
			try {
				helper = new MimeMessageHelper(message, true, Constants.CHARSET);
				try {
					helper.setFrom(mailSender.getUsername());
				} catch (Exception e) {
					e.printStackTrace();
				}
				if (StringUtils.isNotBlank(subject)) {
					helper.setSubject(subject);
				} else {
					logger.warn("Sending message with empty subject.");
				}
					helper.setText(body, true);
					String toAddress = getStudentFeedEmailNotificationAddress();
					String[] feedEmailIds = toAddress.split(",");
					
					if (StringUtils.isNotBlank(toAddress)) {
//						helper.addTo(toAddress);
						if (feedEmailIds.length > 0) {
							for (String emailId : feedEmailIds) {
								try {
									helper.addTo(emailId);
								} catch (Exception ex) {
									logger.warn("Could not set to address:", ex);
								}
							}
						}
						notificationLog.setIsSuccess("Y");
						notificationLog.setErrorMessage(null);
					} else {
						notificationLog.setIsSuccess("N");
						notificationLog.setErrorMessage("No Recipient Address");
					}
				
				executorService.execute(() -> mailSender.send(message));
				logger.info("Mail sending processed");
			} catch (MessagingException ex) {
				logger.error("Failed to create mime message helper.", ex);
				logDetailsInFile("Email service call exception "+ex+ " at");
			}
		} else {
			logger.info("Failed to send email due to inability to obtain valid email mailSender, please check your configuration.");
			notificationLog.setIsSuccess("N");
			notificationLog.setErrorMessage("Failed to send email due to inability to obtain valid email mailSender, please check your configuration.");
		}
	}
	
	public boolean checkRoleForPerson(String unitNumber, String personId, Integer roleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "select count(*) from PersonRoles where personId =: personId and unitNumber =: unitNumber and roleId =: roleId";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("personId", personId);
		query.setParameter("unitNumber", unitNumber);
		query.setParameter("roleId", roleId);
		Integer count = Integer.parseInt(query.getSingleResult().toString());
		return count == 0 ? true : false;
	}
	
	private String getStudentFeedEmailNotificationAddress() {
		return commonDao.getParameterValueAsString("STUDENT_FEED_RECIPIENT");
	}

	private void assignStudentRole(Person person, Integer roleId) {
		try {
			if (checkRoleForPerson(person.getHomeUnit(), person.getPersonId(), roleId)) {
				PersonRoles personRole = new PersonRoles();
				personRole.setPersonId(person.getPersonId());
				personRole.setRoleId(roleId);
				personRole.setUnitNumber(person.getHomeUnit());
				personRole.setDescentFlag(Constants.NO);
				personRole.setUpdateTimeStamp(updateTimestamp);
				personRole.setUpdateUser("quickstart");
				hibernateTemplate.saveOrUpdate(personRole);
			}
		}catch (Exception e) {
			e.printStackTrace();
		}
	}

}
