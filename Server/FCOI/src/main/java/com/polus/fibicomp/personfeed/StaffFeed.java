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
import java.util.HashSet;
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
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.commons.collections4.CollectionUtils;
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

import com.polus.fibicomp.codetable.dao.JSONParser;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.pojo.Manpower;
import com.polus.fibicomp.manpowerintegration.dao.ManpowerIntegrationDao;
import com.polus.fibicomp.notification.pojo.NotificationLog;
import com.polus.fibicomp.orcid.dao.OrcidDao;
import com.polus.fibicomp.person.controller.PersonController;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.roles.pojo.PersonRoles;

import oracle.jdbc.OracleTypes;

@Component
@Transactional
public class StaffFeed {
	
	protected static Logger logger = LogManager.getLogger(StaffFeed.class.getName());
	
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
	
	@Value("${person.temp.password}")
	private String personTempPassword;

	@Value("${person.feed.api}")
	private String personFeedAPI;
	
	@Value("${person.feed.api.token}")
	private String personFeedAccessToken;
	
	@Value("${person.feed.api.token.type}")
	private String personFeedAccessTokenType;
	
	@Value("${person.feed.retrycount}")
	private Integer retryCount;

	@Value("${person.feed.onoff}")
	private String personfeed_on_off;  
	
	@Autowired
	private PersonController personController;

	@Autowired
	public OrcidDao orcidDao;

	Timestamp updateTimestamp;

	@Autowired
	private ManpowerIntegrationDao manpowerIntegrationDao;

	@Autowired
	private PersonDao personDao;

	@SuppressWarnings("unchecked")
	@Scheduled(cron = "${person.feed.api.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public synchronized void startPersonFeed() throws Exception {

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
		int successCount = 0;
		if (commonDao.getParameterValueAsBoolean(Constants.PERSON_FEED_API_ENABLE)) {
			if (personfeed_on_off.equalsIgnoreCase("on")) {
				try {
					logger.info("Staff feed API execution starts at :" + commonDao.getCurrentTimestamp());
					emailBody.append("Staff Feed API Summary :<br/>");
					logDetailsInFile("Staff feed API call starts at");
					HttpURLConnection connection = null;
					do {
						try {
							logger.info("API calling count :" + count + "at : " + commonDao.getCurrentTimestamp());
							URL urls = new URL(personFeedAPI);
							connection = (HttpURLConnection) urls.openConnection();
							connection.setRequestMethod("GET");
							connection.setRequestProperty("Accept", "application/json");
							connection.setRequestProperty(personFeedAccessTokenType, personFeedAccessToken);
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
					int totalCountofData = persons.size();
					logger.info("Total count of records: " + totalCountofData);
					logDetailsInFile("Total count of person records from API: " + totalCountofData + " at");
					emailBody.append("<br/> Total count of staff records: " + totalCountofData);
					logDetailsInFile("Processing of staff feed API records to Fibi starts at");
					logger.info("Processing of staff feed data to Fibi starts at: ");
					updateTimestamp = commonDao.getCurrentTimestamp();
					List<Person> personList = getStaffPersons();
					Map<String, List<Person>> personMap = personList.stream().collect(Collectors.groupingBy(Person::getPersonId));
					for (int index = 0; index < totalCountofData; index++) {
						//Person person = new Person();
						Boolean isExternalUser = Boolean.FALSE;
						HashMap<String, Object> hmResult = (HashMap<String, Object>) persons.get(index);
						String personsId = hmResult.get("personid").toString();
					    if (Boolean.FALSE.equals(checkPersonIsExternal(personMap, personsId, isExternalUser))) {
						Person person = personMap.get(personsId) != null && !personMap.get(personsId).isEmpty() ? personMap.get(personsId).get(0) : new Person();
						//String personsId = hmResult.get("personid").toString();

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
						String orcidId = hmResult.get("orciD_ID").toString();
						if (orcidId != null && !orcidId.equalsIgnoreCase("null")) {
							person.setOrcidId(orcidId);
						}
						String accessToken = hmResult.get("accesS_TOKEN").toString();
						if (accessToken != null && !accessToken.equalsIgnoreCase("null")) {
							person.setAccessToken(accessToken);
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
								personDao.fetchPersonsByLikeUserName(userName).forEach(personData -> {
									personDao.updateOldPersonUsername(personData.getPersonId());
								});
								person.setPrincipalName(userName);
//								duplicateUserNames.add(person);
//								logDetailsInFile("Staff record with user name = NULL for PersonId: "
//										+ person.getPersonId() + " at");
							}
						}
						person.setPassword(personTempPassword);
						String unitNumber = hmResult.get("homeunit").toString();
						if (unitNumber != null && !unitNumber.equalsIgnoreCase("null")) {
//						logger.info("In Time valid check Unit : "+unitNumber+" at : " + commonDao.getCurrentTimestamp());
							// logDetailsInFile("Checking is valid Unit or not for - " + unitNumber + ", for
							// PersonId : " + person.getPersonId());
							if (checkValidUnitOrNot(unitNumber)) {
								person.setHomeUnit(unitNumber);
							} else {
								if (person.getIsFaculty() == true) {
									nonValidPersonUnits.add(person);
									logDetailsInFile("Invalid Unit : " + unitNumber + ", for PersonId : "
											+ person.getPersonId() + " at");
								}
							}
							// logDetailsInFile("Checking completed for Unit : " + unitNumber + ", for
							// PersonId: " + person.getPersonId());
//						logger.info("Out Time valid check Unit : "+unitNumber+" at : " + commonDao.getCurrentTimestamp());
						} else {
							if (person.getIsFaculty() == true) {
								nonValidPersonUnits.add(person);
								logDetailsInFile("Null Unit for personID : " + person.getPersonId() + " at");
							}
						}
						try {
//							logger.info("checkPersonDataTruncation in time at : " + commonDao.getCurrentTimestamp());
//						logDetailsInFile("check for PersonDataTruncation starts "+", for PersonId: " + person.getPersonId() +"at : ");
							if (checkPersonDataTruncation(person)) {
								// logDetailsInFile("check for PersonDataTruncation ended "+", for PersonId: " +
								// person.getPersonId() +"at : ");
//								String responseResult = processPersonInsertion(person);	
								// logger.info("Inserting person to datatbase at : " + commonDao.getCurrentTimestamp());
								// logDetailsInFile("Inserting person to database "+", for PersonId:" +
								// person.getPersonId() +"at : ");
								// insert staff record to db
								String responseResult = personController.savePersonFromFeed(person);
								// logger.info("Insertion of person done at : " + commonDao.getCurrentTimestamp());
								// logDetailsInFile("Insertion of person done for PersonId:" +
								// person.getPersonId() +"at : ");
								if (responseResult != null && responseResult.equalsIgnoreCase("Success")) {
									assignRoles(person);
									successCount = successCount + 1;
								} else {
									logDetailsInFile("Insertion of staff record failed for PersonId:"
											+ person.getPersonId() + " at");
									failedPersons.add(person);
								}
							} else {
								// logDetailsInFile("check for PersonDataTruncation done "+", for PersonId: " +
								// person.getPersonId() +"at : ");
								logDetailsInFile("Invalid staff record found for person with personId :"
										+ person.getPersonId() + " at");
								nonValidPersons.add(person);
							}
//								logger.info("checkPersonDataTruncation out time at : " + commonDao.getCurrentTimestamp());  
						} catch (Exception e) {
							// emailBody.append("<br/> Exception occured while insertion of person with
							// PersonId: " + person.getPersonId()+ "<br/>" + e + "<br/>" + " at : " +
							// commonDao.getCurrentTimestamp());
							logDetailsInFile("Exception occured while insertion of staff with PersonId: "
									+ person.getPersonId() + "<br/>" + e + "<br/>" + " at");
							e.printStackTrace();
						}
//					personId = person.getPersonId();
				    updateManpowerDetailsbyPerson(person);
						}
					}
					connection.disconnect();
					logDetailsInFile("Processing of person feed records to Fibi completed at");
					logger.info(
							"Processing of person feed data to Fibi completed at: " + commonDao.getCurrentTimestamp());
					// calling procedure to set persons inactive
					logger.info("Time in remove Inactive Persons: " + commonDao.getCurrentTimestamp());
					logDetailsInFile("Process to set persons Inactive started at");
					Integer inactivePersonRecords = callDeactivatePersons();
					logger.info("Time out remove Inactive Persons: " + commonDao.getCurrentTimestamp());
					logDetailsInFile("Process to set persons Inactive ended at");
					logDetailsInFile("Success Count : " + successCount + " at");
					emailBody.append("<br/>Success Count : " + successCount);
					emailBody.append("<br/>Failed Count :" + (totalCountofData - successCount));
					logDetailsInFile("Failed Count:" + (totalCountofData - successCount) + " at");
					// sync person Roles after assigning roles !!!
					logDetailsInFile("Sync PersonRole started at");
					refreshPersonRoleRT();
					logDetailsInFile("Sync PersonRole ended at");
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
				logger.info("Staff feed API call ends at :" + commonDao.getCurrentTimestamp());
				logDetailsInFile("Staff feed API call ends at");
//			logger.info("emailServiceCall in : " + commonDao.getCurrentTimestamp());
				logDetailsInFile("emailService Call at");
				emailServiceCall(emailBody.toString(), nonValidPersonUnits, nonValidPersons, failedPersons,
						duplicateUserNames, invalidFacultys, invalidResearchStaffs, invalidSupportStaffs);
				logDetailsInFile("STAFF RECORD FEED PROCESSES FINISHED AT");
			}
		}
	}
	
	private List<Person> getStaffPersons() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Person> query = builder.createQuery(Person.class);
		Root<Person> rootPerson = query.from(Person.class);	
		Predicate predicateIsGraduateStudentStaffNull = rootPerson.get("isGraduateStudentStaff").isNull();
		Predicate predicateIsGraduateStudentStaff = builder.equal(rootPerson.get("isGraduateStudentStaff"), false);
		query.where(builder.or(predicateIsGraduateStudentStaff, predicateIsGraduateStudentStaffNull));
		return session.createQuery(query).getResultList();
	}

	private void updateManpowerDetailsbyPerson(Person person) {
		try {
			if (person != null && person.getPersonId() != null) {
				Manpower manpowerDetail = personDao.getManpowerDetailsbyPersonId(person.getPersonId());
				if (manpowerDetail != null && manpowerDetail.getManpowerPersonId() != null) {
					manpowerDetail.setFullName(person.getFullName());
					manpowerDetail.setManpowerPersonId(person.getPersonId());
					manpowerDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
					manpowerIntegrationDao.saveOrUpdate(manpowerDetail);
					List<AwardManpowerResource> manpowerResources = manpowerIntegrationDao.getManpowerResourcesbyPersonIdAndWithoutName(manpowerDetail.getManpowerPersonId());
					manpowerResources.forEach(manpowerResource -> {
						manpowerResource.setFullName(person.getFullName());
						manpowerResource.setUpdateTimestamp(commonDao.getCurrentTimestamp());
						manpowerIntegrationDao.saveOrUpdateAwardManpowerResource(manpowerResource);
					});
				}
			}
		} catch(Exception e) {
			logger.info("Exception updateManpowerDetailsbyPerson :" + commonDao.getCurrentTimestamp());
		}
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

	private void assignRoles(Person person) {
		if (person.getHomeUnit() != null) {
			if (person.getIsFaculty() == true) {
//					logger.info("In Time assign Person Role for PersonId: " + person.getPersonId()+ " at : " + commonDao.getCurrentTimestamp());
				// logDetailsInFile("Assign PI role starts for PersonId : " +
				// person.getPersonId()+ " at :");
				assignPersonRole(person, Constants.PI_RESEARCH_ASSISTANTS);
//					logger.info("Out Time assign Person Role for PersonId: " + person.getPersonId()+ " at : " + commonDao.getCurrentTimestamp());
				// logDetailsInFile("Assign PI role ends for PersonId : " +
				// person.getPersonId()+ " at :");
			}
			if (person.getIsResearchStaff() == true) {
//					logger.info("In Time assign Create proposal Role for PersonId: " + person.getPersonId()+ " at : " + commonDao.getCurrentTimestamp());
				// logDetailsInFile("Assign create proposal role starts for PersonId : "+
				// person.getPersonId() + " at :");
				assignPersonRole(person, Constants.CREATE_PROPOSAL);
//					logger.info("Out Time assign Create proposal Role for PersonId: " + person.getPersonId()+ " at : " + commonDao.getCurrentTimestamp());
				// logDetailsInFile("Assign create proposal role ended for PersonId : "+
				// person.getPersonId() + " at :");
			}
		}
	}

	private Boolean checkPersonIsExternal(Map<String, List<Person>> personMap, String personsId, Boolean isExternalUser) {
		if (personMap.get(personsId) != null && !personMap.get(personsId).isEmpty()) {
			if(personMap.get(personsId).get(0).getIsExternalUser() != null && personMap.get(personsId).get(0).getIsExternalUser()) {
				isExternalUser = Boolean.TRUE;
			} 
		}
		return isExternalUser;
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
				sbuffer.append("<br/><br/>Person records with invalid & null units are listed below : <br/> PersonIds : ");
				sbuffer.append("[ ");
				for (Person person : nonValidPersonUnits) {
					sbuffer.append("" + person.getPersonId());
					sbuffer.append(",");
				}
				sbuffer.deleteCharAt(sbuffer.length() - 1);
				sbuffer.append(" ]");
			}
			if (nonValidPersons != null && !nonValidPersons.isEmpty()) {
				sbuffer.append("<br/><br/>Person records with invalid data are listed below : <br/> PersonIds : ");
				sbuffer.append("[ ");
				for (Person person : nonValidPersons) {
					sbuffer.append("" + person.getPersonId());
					sbuffer.append(",");
				}
				sbuffer.deleteCharAt(sbuffer.length() - 1);
				sbuffer.append(" ]");
			}
			if (insertionFailedPersons != null && !insertionFailedPersons.isEmpty()) {
				sbuffer.append("<br/><br/>Person records failed on insertion are listed below : <br/> PersonIds : ");
				sbuffer.append("[ ");
				for (Person person : insertionFailedPersons) {
					sbuffer.append("" + person.getPersonId());
					sbuffer.append(",");
				}
				sbuffer.deleteCharAt(sbuffer.length() - 1);
				sbuffer.append(" ]");
			}
			if (duplicateUserNames != null && !duplicateUserNames.isEmpty()) {
				sbuffer.append("<br/><br/>Person records with duplicate username are listed below : <br/> PersonIds : ");
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
				sbuffer.append("<br/><br/>Person records with invalid Faculty are listed below : <br/> PersonIds : ");
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
				sbuffer.append("<br/><br/>Person records with invalid Research staff are listed below : <br/> PersonIds : ");
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
				sbuffer.append("<br/><br/>Person records with invalid Support staff are listed below : <br/> PersonIds : ");
				sbuffer.append("[ ");
				for (Person person : invalidSupportStaffs) {
					sbuffer.append("" + person.getPersonId());
					sbuffer.append(",");
				}
				sbuffer.deleteCharAt(sbuffer.length() - 1);
				sbuffer.append(" ]");
			}

			emailBody = emailBody + sbuffer.toString();
			Set<String> hash_Set = new HashSet<>();
		    hash_Set.add("");
			sendEmail(hash_Set, "Staff Feed Report", emailBody);
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
					statement.setString(1, "STAFF");
					statement.setTimestamp(2, updateTimestamp);
					statement.execute();
					resultSet = statement.getResultSet();
				} else if (oracledb.equalsIgnoreCase(Constants.dbOracle)) {
					String procedureName = "DELETE_INACTIVE_PERSONS";
					String functionCall = "{call " + procedureName + "(?,?,?)}";
					statement = connection.prepareCall(functionCall);
					statement.registerOutParameter(1, OracleTypes.CURSOR);
					statement.setString(2, "STAFF");
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
					logDetailsInFile("Person Data Truncation occured for PersonId: " + person.getPersonId()+" at");
					return false;
				}
			}
			if (!(person.getFullName().length() <= 90)) {
				logDetailsInFile("Person Data Truncation occured at Full Name: " + person.getFullName()+ " for PersonId: " + person.getPersonId()+" at");
				return false;
			}
			if (person.getPrincipalName() != null) {
				if (!(person.getPrincipalName().length() <= 51)) {
					logDetailsInFile("Person Data Truncation occured at User Name: " + person.getPrincipalName()+ "for PersonId: " + person.getPersonId()+" at");
					return false;
				}
			}
			if (person.getEmailAddress()!= null) {
			if (!(person.getEmailAddress().length() <= 60)) {
				logDetailsInFile("Person Data Truncation occured at email Address: "+ person.getEmailAddress() + "for PersonId: " + person.getPersonId()+" at");
				return false;
			}
			}
			if (person.getHomeUnit() != null) {
				if (!(person.getHomeUnit().length() <= 8)) {
					logDetailsInFile("Person Data Truncation occured at Unit Number: "+ person.getHomeUnit() + " for PersonId: " + person.getPersonId()+" at");
					return false;
				}
			}
			if (person.getPrimaryTitle() != null) {
				if (!(person.getPrimaryTitle().length() <= 51)) {
					logDetailsInFile("Person Data Truncation occured at PrimaryTitle :" + person.getPrimaryTitle()+ " for PersonId:" + person.getPersonId()+" at");
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
				String fileName = "Staff_Feed_Logs";
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
		
		public void sendEmail(Set<String> toAddresses, String subject, String body) {
			logger.info("Received request for mail sending in Staff Feed...");
			NotificationLog notificationLog = new NotificationLog();
			if (mailSender != null) {
				if (CollectionUtils.isEmpty(toAddresses)) {
					return;
				}
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
				notificationLog.setSubject("PersonFeedLog");
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
					if (isEmailTestEnabled()) {
						helper.setText(body, true);
//						helper.setText(getTestMessageBody(body, toAddresses), true);
//						String toAddress = "support@polussoftware.com";
						String toAddress = getPersonFeedEmailNotificationAddress();
						String[] feedEmailIds = toAddress.split(",");
//						notificationLog.setToUserEmailId(toAddress);
						notificationLog.setToUserEmailId(toAddresses.toString());
						if (toAddresses.isEmpty()) {
							notificationLog.setIsSuccess("N");
							notificationLog.setErrorMessage("No Recipients");
						} else {
							notificationLog.setIsSuccess("Y"); 
							notificationLog.setErrorMessage(null);
						}
						if (StringUtils.isNotBlank(toAddress)) {
//							helper.addTo(toAddress);
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
					} else {
						helper.setText(body);
						if (CollectionUtils.isNotEmpty(toAddresses)) {
							for (String toAddress : toAddresses) {
								try {
									helper.addTo(toAddress);
								} catch (Exception ex) {
									logger.warn("Could not set to address:", ex);
								}
							}
						}
					}
					executorService.execute(() -> mailSender.send(message));
					logger.info("Mail sending processed");
				} catch (MessagingException ex) {
					logger.error("Failed to create mime message helper.", ex);
					logDetailsInFile("Email service call exception "+ex+" at");
				}
			} else {
				logger.info("Failed to send email due to inability to obtain valid email mailSender, please check your configuration.");
				notificationLog.setIsSuccess("N");
				notificationLog.setErrorMessage("Failed to send email due to inability to obtain valid email mailSender, please check your configuration.");
			}
		}
		
		private boolean isEmailTestEnabled() {
			//return commonDao.getParameterValueAsBoolean("EMAIL_NOTIFICATION_TEST_ENABLED");
			return true;
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
		
		private String getPersonFeedEmailNotificationAddress() {
			return commonDao.getParameterValueAsString("PERSON_FEED_RECIPIENT");
		}

		private void assignPersonRole(Person person, Integer roleId) {
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
