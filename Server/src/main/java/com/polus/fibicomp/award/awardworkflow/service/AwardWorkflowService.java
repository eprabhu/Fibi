package com.polus.fibicomp.award.awardworkflow.service;

import java.util.List;
import java.util.Set;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;

@Transactional
@Service(value = "awardWorkflowService")
public interface AwardWorkflowService {

	/**
	 * This method is used to submit Award.
	 * @param awardVO - object of awardVO.
	 * @return a String of details of Award.
	 */
	public AwardVO submitAward(AwardVO awardVO);

	/**
	 * This method is used to add Alternative Approver
	 * @param awardVO - object of awardVO.
	 * @return a String of details of Award.
	 */
	public String addAlternativeApprover(AwardVO vo);

	/**
	 * This method is used to get award principal investigator
	 * @param awardPersons - list of awardPerson.
	 * @return AwardPerson.
	 */
	public AwardPerson getAwardPrincipalInvestigator(List<AwardPerson> awardPersons);

	/**
	 * This method is used to add a new sequential stop in workflow map
	 * @param vo
	 * @return updated workflow list
	 */
	public String addSequentialStop(AwardVO vo);

	/**
	 * This method is used to send service request notification.
	 * @param awardVO - Object of AwardVO class.
	 * @param notificationTypeId
	 * @param dynamicEmailRecipients
	 */
	public void sendNotification(AwardVO awardVO, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients);

	/**
	 * This method is used to update workflow status and sequence status based on the sap response
	 * @param award
	 * @param updateUser
	 */
	public void sapBasedWorkflowStatusAndSequenceStatus(Award award, String updateUser);

	/**
	 * This method is used to feed the sap award details
	 * @param award
	 * @param userName
	 */
	public void checkAndFeedSapAwardDetail(Award award, String userName);

	/**
	 * @param award
	 */
	public void updateAwardHistoryLog(Award award);
}
