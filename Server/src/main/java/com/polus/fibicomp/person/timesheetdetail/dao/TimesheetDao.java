package com.polus.fibicomp.person.timesheetdetail.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardKeyPersonTimesheet;
import com.polus.fibicomp.person.timesheetdetail.vo.TimesheetVO;

@Transactional
@Service
public interface TimesheetDao {

	/**
	 * This method is used get award details by params
	 * @param vo
	 * @return.
	 */
	public List<Award> getAwardTimesheetByPersonId(TimesheetVO vo);

	/**
	 * This method is used to save or update award key person time sheet data.
	 * @param awardKePersonTimesheet
	 * @return object of awardKePersonTimesheet
	 */
	public AwardKeyPersonTimesheet saveOrUpdateAwardKeyPersonTimesheet(AwardKeyPersonTimesheet awardKeyPersonTimesheet);

	/**
	 * This method is used to get award key person time sheet data.
	 * @param awardPersonId
	 * @param awardId
	 * @return list of awardKePersonTimesheet
	 */
	public List<AwardKeyPersonTimesheet> getAwardKeyPersonTimesheetByParams(Integer awardPersonId, Integer awardId);

	/**
	 * This method is used to delete award key person timesheet 
	 * @param awardKeyPersonTimesheet - object of AwardKeyPersonTimesheet
	 */
	public void deleteAwardKeyPersonTimesheet(AwardKeyPersonTimesheet awardKeyPersonTimesheet);

	/**
	 * This method is used to get award key person timesheet type by award id
	 * @param awardId
	 * @return timesheet type
	 */
	public String getAwardKeyPersonTimesheetType(Integer awardId);

	/**
	 * This method is used to delete award key person timesheet by params
	 * @param awardPersonId 
	 * @param awardId
	 */
	public void deleteAwardKeyPersonTimesheetByParams(Integer awardPersonId, Integer awardId);

}
