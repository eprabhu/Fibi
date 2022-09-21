package com.polus.fibicomp.login.service;

import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dashboard.service.DashboardService;
import com.polus.fibicomp.login.dao.LoginDao;
import com.polus.fibicomp.login.pojo.PersonLoginDetail;
import com.polus.fibicomp.login.pojo.PersonSystemNotificationMapping;
import com.polus.fibicomp.login.pojo.SystemNotification;
import com.polus.fibicomp.login.vo.LoginVO;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.PersonDTO;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.roles.dao.AuthorizationServiceDao;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service(value = "loginService")
public class LoginServiceImpl implements LoginService {

	protected static Logger logger = LogManager.getLogger(LoginServiceImpl.class.getName());

	@Autowired
	private LoginDao loginDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private DashboardService dashboardService;
	
	@Autowired
	private AuthorizationServiceDao authorizationServiceDao;
	
	@Autowired
	public PersonDao personDao;

	@Override
	public PersonDTO loginCheck(String loginMode, String userName, String password, HttpServletRequest request, HttpServletResponse response) throws Exception {
		Person person = null;
		PersonDTO personDTO = new PersonDTO();
		if ("USERID".equalsIgnoreCase(loginMode)) {
			person = loginDao.authenticate(userName, password);
		}
		if (person != null) {
			personDTO = loginDao.readPersonData(userName);
			personDTO.setLogin(true);
		}
		return personDTO;
	}

	/*@Override
	public boolean isUnitAdmin(String personId) {
		return loginDao.isUnitAdmin(personId);
	}*/

	@Override
	public String changePassword(CommonVO vo) throws Exception {
		logger.info("personId :" + vo.getPersonId());
		Person person = loginDao.getCurrentPassword(vo.getPersonId());
		String oldPassword = commonService.hash(vo.getPassword());
		if (oldPassword.equals(person.getPassword())) {
			String encryptedPWD = commonService.hash(vo.getNewPassword());
			Integer result = loginDao.changePassword(encryptedPWD, vo.getPersonId());
			if (result == 1) {
				vo.setUpdatePasswordMessage(Constants.UPDATE_PASSWORD_SUCCESS_MESSAGE);
			} else {
				vo.setUpdatePasswordMessage(Constants.UPDATE_PASSWORD_ERROR_MESSAGE);
			}
		} else {
			vo.setOldPasswordErrorMessage(Constants.OLD_PASSWORD_ERROR_MESSAGE);
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String fetchPersonLoginDetails(LoginVO loginVO) {
		loginDao.getPersonLoginDetails(loginVO,loginVO.getUnitList());
		if(loginVO.getPersonLoginDetails() != null && !loginVO.getPersonLoginDetails().isEmpty()) {
			getUnitOfPerson(loginVO.getPersonLoginDetails());
		}
		return commonDao.convertObjectToJSON(loginVO);
	}
	
	private void getUnitOfPerson(List<PersonLoginDetail> persons) {
		Set<String> personIds = persons.stream().map(PersonLoginDetail::getPersonId).collect(Collectors.toSet());
		if (!personIds.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByPersonId(new ArrayList<>(personIds));
			Map<String, Unit> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPersonId(), person -> person.getUnit() != null ? person.getUnit() : new Unit()));
			persons.stream().filter(item -> item.getPersonId() != null).filter(item -> collect.containsKey(item.getPersonId())).forEach(item -> item.setUnit(collect.get(item.getPersonId())));
		}
	}

	@Override
	public void savePersonLoginDetails(String personID, String fullName, String loginStatus, String userName) {
		PersonLoginDetail personLoginDetail = new PersonLoginDetail();
		personLoginDetail.setPersonId(personID);
		personLoginDetail.setLoginStatus(loginStatus);
		personLoginDetail.setFullName(fullName);
		personLoginDetail.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		personLoginDetail.setUpdateUser(userName);
		loginDao.savePersonLoginDetail(personLoginDetail);
	}

	@Override
	public ResponseEntity<byte[]> exportUserActivityDatas(LoginVO loginVO) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			CommonVO vo = new CommonVO();
			vo.setExportType("xlsx");
			vo.setDocumentHeading(loginVO.getDocumentHeading());
			XSSFWorkbook workbook = new XSSFWorkbook();
			List<Object[]> dashboardData = new ArrayList<>();
			loginVO = loginDao.getPersonLoginDetails(loginVO,loginVO.getUnitList());
			if(loginVO.getPersonLoginDetails() != null && !loginVO.getPersonLoginDetails().isEmpty()){
				getUnitOfPerson(loginVO.getPersonLoginDetails());
				List<PersonLoginDetail> personLoginDetails = loginVO.getPersonLoginDetails();
				for (PersonLoginDetail personLoginDetail: personLoginDetails) {
					Object[] object = new Object[5];
					object[0] = personLoginDetail.getPersonId();
					object[1] = personLoginDetail.getFullName();
					object[2] = personLoginDetail.getUnit().getUnitName();
					object[3] = personLoginDetail.getLoginStatus();
					DateFormat dateFormat = new SimpleDateFormat(Constants.TWELVE_HOUR_DATE_FORMAT);
					dateFormat.setTimeZone(TimeZone.getTimeZone(Constants.CRON_JOB_TIMEZONE));
					object[4] = dateFormat.format(personLoginDetail.getUpdateTimestamp());
					dashboardData.add(object);
				}
			}
			XSSFSheet sheet = workbook.createSheet("User Details");
			commonService.addDetailsInHeader(workbook,sheet);
			Object[] tableHeadingRow = { "Id#", "Name", "Unit", "Activity" , "Date And Time" };
			dashboardService.prepareExcelSheet(dashboardData, sheet, tableHeadingRow, workbook, vo);
			attachmentData = dashboardService.getResponseEntityForDownload(vo, workbook);
		} catch (Exception e) {
			logger.error(e);
		}
		return attachmentData;
	}

	@Override
	public String getUnitByPersonId(LoginVO vo, String personId) {
		List<Unit> unitLists = null;
		List<String> systemRights = authorizationServiceDao.allSystemLevelPermission(personId);
		if (systemRights != null && !systemRights.isEmpty()) {
			unitLists = loginDao.getUnitsListByPersonIdAndRights(vo.getPersonId(), systemRights);
		}
		return commonDao.convertObjectToJSON(unitLists);
	}

	@Override
	public String fetchSystemAllNotification() {
		return commonDao.convertObjectToJSON(prepareSystemNotification());
	}

	private LoginVO prepareSystemNotification() {
		LoginVO vo = new LoginVO();
		List<SystemNotification> systemNotifications = loginDao.fetchSystemAllNotification(Timestamp.valueOf((commonDao.getCurrentTimestamp().toString().substring(0, 11)).concat(Constants.START_TIME)));
		if (systemNotifications != null && !systemNotifications.isEmpty()) {
			systemNotifications.forEach(systemNotification -> {
				systemNotification.setPersonSystemNotificationMapping(loginDao.getPersonSystemNotificationByNotificationId(systemNotification.getSystemNotificationId(), AuthenticatedUser.getLoginPersonId()));
			});
		}
		vo.setSystemNotifications(systemNotifications);
		return vo;
	}

	@Override
	public String markNotificationsAsRead(LoginVO vo) {
		if (Boolean.TRUE.equals(vo.getSystemNotificationRead())) {
			PersonSystemNotificationMapping personSystemNotificationMapping = new PersonSystemNotificationMapping();
			personSystemNotificationMapping.setPersonId(AuthenticatedUser.getLoginPersonId());
			personSystemNotificationMapping.setSystemNotificationId(vo.getSystemNotificationId());
			personSystemNotificationMapping.setReadTimestamp(commonDao.getCurrentTimestamp());
			loginDao.saveOrUpdatePersonSystemNotificationMapping(personSystemNotificationMapping);
		} else {
			loginDao.deletePersonSystemNotificationMapping(vo.getSystemNotificationId(), AuthenticatedUser.getLoginPersonId());
		}
		return commonDao.convertObjectToJSON(prepareSystemNotification());
	}

}
