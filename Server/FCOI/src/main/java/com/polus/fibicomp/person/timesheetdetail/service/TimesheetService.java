package com.polus.fibicomp.person.timesheetdetail.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.person.timesheetdetail.vo.TimesheetVO;

@Transactional
@Service
public interface TimesheetService {

	/**
	 * This method is used get award details by params
	 * @param vo
	 * @return.
	 */
	public String loadAwardTimesheetByPersonId(TimesheetVO vo);

	/**
	 * This method is used to save or update award key person time sheet data.
	 * @param AwardVo - vo
	 * @return
	 */
	public String saveOrUpdateAwardKeyPersonTimesheet(TimesheetVO vo);

	/**
	 * This method is used to get award key person time sheet data.
	 * @param AwardVo - vo
	 * @return
	 */
	public String getAwardKeyPersonTimesheetDetails(TimesheetVO vo);

	/**
	 * This method is used to prepare award key person timesheet data
	 * @param vo
	 * @param awardPersonId
	 * @param awardId
	 * @return vo
	 */
	public TimesheetVO prepareKeyPersonTimesheetData(TimesheetVO vo, Integer awardPersonId, Integer awardId);

	/**
	 * This method is used to check right permission.
	 * @param leadUnitNumber - leadUnitNumber
	 * @param moduleItemKey  -moduleItemKey
	 * @param rightName - rightName
	 * @return 
	 */
	public boolean checkRightPermissionByParam(String leadUnitNumber, Integer moduleItemKey, String rightName);

}
