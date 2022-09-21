package com.polus.fibicomp.integration.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.integration.vo.IntegrationVO;

@Transactional
@Service(value = "integrationService")
public interface IntegrationService {

	/**
	 * This method is used to add Award Hours Log RT.
	 * @param vo - Object of IntegrationVO class.
	 * @return Set of values as message.
	 */
	public String addAwardHoursLogRT(IntegrationVO vo);

	/**
	 * This method is used to add Expense Tracker RT.
	 * @param vo - Object of IntegrationVO class.
	 * @return Set of values as message.
	 */
	public String addExpenseTrackerRT(IntegrationVO vo);

	/**
	 * This method is used to add Publication.
	 * @param vo - Object of IntegrationVO class.
	 * @return Set of values as message.
	 */
	public String addPublication(IntegrationVO vo);

	/**
	 * This method is used to delete Award Hours Log RT.
	 * @return delete success/fail message.
	 */
	public String deleteAwardHoursLogRT();

	/**
	 * This method is used to delete expense tracker RT.
	 * @return delete success/fail message.
	 */
	public String deleteExpenseTrackerRT();

	/**
	 * This method is used to save SAP AwardFeed details of a active award.
	 * @param awardId - awardId
	 * @param awardNumber - awardNumber
	 * @param sequenceNumber - sequenceNumber
	 * @param updateUser - updateUser
	 */
	public void saveSAPAwardDetails(Integer awardId, String awardNumber, Integer sequenceNumber, String updateUser);

	/**
	 * This method is used to feed the new awards based on the given seed award id
	 * @param vo
	 * @return success message
	 */
	public String seedAward(IntegrationVO vo);

	/**
	 * This method is used to feed the new proposals based on the given seed proposal id
	 * @param vo
	 * @return success message
	 */
	public String feedProposal(IntegrationVO vo);

	/**
	 * This method is used to check for the sap feed
	 * @param awardNumber
	 * @param awardId 
	 * @return true- if can add sap feed else false
	 */
	public String checkForTheSAPFeedResponse(String awardNumber, Integer awardId);

}
