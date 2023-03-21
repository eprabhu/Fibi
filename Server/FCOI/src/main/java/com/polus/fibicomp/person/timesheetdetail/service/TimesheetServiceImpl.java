package com.polus.fibicomp.person.timesheetdetail.service;

import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardKeyPersonTimesheet;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.timesheetdetail.dao.TimesheetDao;
import com.polus.fibicomp.person.timesheetdetail.vo.TimesheetVO;
import com.polus.fibicomp.security.AuthenticatedUser;

@Transactional
@Service(value = "timesheetService")
public class TimesheetServiceImpl implements TimesheetService {

	protected static Logger logger = LogManager.getLogger(TimesheetServiceImpl.class.getName());

	@Autowired
	private TimesheetDao timesheetDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	public PersonDao personDao;

	@Autowired
	private GrantCallDao grantCallDao;

	private static final String VIEW_KEY_PERSON_TIMESHEET_RIGHT_NAME = "VIEW_KEY_PERSON_TIMESHEET";
    private static final String MAINTAIN_KEY_PERSON_TIMESHEET_RIGHT_NAME = "MAINTAIN_KEY_PERSON_TIMESHEET";
   
	@Override
	public String loadAwardTimesheetByPersonId(TimesheetVO vo) {
		List<Award> awards = timesheetDao.getAwardTimesheetByPersonId(vo);
		if (awards != null && !awards.isEmpty()) {
			awards.forEach(award -> {
				if (award.getAwardPersons() != null && !award.getAwardPersons().isEmpty()) {
					award.getAwardPersons().stream().filter(awardPerson -> vo.getPersonId().equals(awardPerson.getPersonId())).forEach(awardPerson -> {
						award.setAwardPersonId(awardPerson.getAwardPersonId());
						award.setPersonRoleName(awardPerson.getProposalPersonRole().getDescription());
					});
				}
				award.setViewTimesheetRightExist(checkRightPermissionByParam(award.getLeadUnitNumber() , award.getAwardId(), VIEW_KEY_PERSON_TIMESHEET_RIGHT_NAME));
				if (award.getGrantHeaderId() != null) {
					award.setGrantCallName(grantCallDao.getGrantCallNameByGrantId(award.getGrantHeaderId()));
				}
			});
		}
		vo.setAwards(awards);
		vo.setAwardTypes(awardDao.fetchAllAwardTypes());
		vo.setPerson(personDao.getPersonDetailById(vo.getPersonId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveOrUpdateAwardKeyPersonTimesheet(TimesheetVO vo) {
		List<AwardKeyPersonTimesheet> awardKeyPersonTimesheets = vo.getAwardKeyPersonTimesheet();
		if (awardKeyPersonTimesheets != null && !awardKeyPersonTimesheets.isEmpty()) {
			awardKeyPersonTimesheets.stream().forEach(awardKeyPersonTimesheet -> {
				if (awardKeyPersonTimesheet.getValue() != null) {
					timesheetDao.saveOrUpdateAwardKeyPersonTimesheet(awardKeyPersonTimesheet);
				} else if (awardKeyPersonTimesheet.getKeypersonTimesheetId() != null) {
					timesheetDao.deleteAwardKeyPersonTimesheet(awardKeyPersonTimesheet);
				}
			});
		}
		vo.setAwardKeyPersonTimesheet(awardKeyPersonTimesheets);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getAwardKeyPersonTimesheetDetails(TimesheetVO vo) {
		setRightsForAwardKeyPersonTimesheet(vo);
		prepareKeyPersonTimesheetData(vo, vo.getAwardPersonId(), vo.getAwardId());
		return commonDao.convertObjectToJSON(vo);
	}

	private TimesheetVO setRightsForAwardKeyPersonTimesheet(TimesheetVO vo) {
		String leadUnitNumber = awardDao.fetchAwardLeadUnitNumberByAwardId(vo.getAwardId());
		vo.setMaintainTimesheetRightExist(checkRightPermissionByParam(leadUnitNumber, vo.getAwardId(), MAINTAIN_KEY_PERSON_TIMESHEET_RIGHT_NAME));
		return vo;
	}

	@Override
	public boolean checkRightPermissionByParam(String leadUnitNumber, Integer moduleItemKey, String rightName) {
		Boolean isRightExist  = Boolean.FALSE;
		if (leadUnitNumber != null) {
			isRightExist = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), rightName, leadUnitNumber);
		}
		Boolean rightFromPerimmisionExist = awardDao.checkPersonHasRightInAward(AuthenticatedUser.getLoginPersonId(), rightName, moduleItemKey);
		return ((isRightExist != null && Boolean.TRUE.equals(isRightExist)) || (rightFromPerimmisionExist != null && Boolean.TRUE.equals(rightFromPerimmisionExist)));
	}

	@Override
	public TimesheetVO prepareKeyPersonTimesheetData(TimesheetVO vo, Integer awardPersonId, Integer awardId) {
		List<AwardKeyPersonTimesheet> awardKeyPersonTimesheets =  timesheetDao.getAwardKeyPersonTimesheetByParams(awardPersonId, awardId);
		LinkedHashMap<String, List<AwardKeyPersonTimesheet>> awardKeyPersonTimesheetGroups = awardKeyPersonTimesheets.stream()
				.sorted(Comparator.comparing(AwardKeyPersonTimesheet:: getYear))
				.collect(Collectors.groupingBy(awardKeyPersonTimesheet -> awardKeyPersonTimesheet.getYear(),LinkedHashMap::new,Collectors.toList()));
		vo.setAwardKeyPersonTimesheetDetails(awardKeyPersonTimesheetGroups);
		String timeSheetType = timesheetDao.getAwardKeyPersonTimesheetType(awardId);
		if (timeSheetType != null) {
			vo.setAwardKeyPersonTimesheetType(timeSheetType);
		} else {
			vo.setAwardKeyPersonTimesheetType(commonDao.getParameterValueAsString(Constants.AWARD_KEY_PERSON_TIMESHEET_TYPE));
		}
		return vo;
	}

}
