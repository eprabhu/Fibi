package com.polus.fibicomp.award.schedule;

import java.sql.Timestamp;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.businessrule.service.BusinessRuleService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;

@Component
@Transactional
public class AwardScheduler {

	protected static Logger logger = LogManager.getLogger(AwardScheduler.class.getName());

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private BusinessRuleService businessRuleService;

	@Autowired
	private WorkflowDao workflowDao;

	@Autowired
	private AwardService awardService;

	@Autowired
	private CommonService commonService;

	@Scheduled(cron = "${award.updateAwardStatus.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void updateAwardStatus() {
		Timestamp currentDate = commonDao.getCurrentTimestamp();
		logger.info("Award Status Scheduler Starts at {}",  currentDate);
		List<Award> awards = awardDao.fetchAllHoldAwards();
		if (awards != null && !awards.isEmpty()) {
			for (Award award : awards) {
				logger.info("AwardId : {}",  award.getAwardId());
				award.setStatusCode(Constants.AWARD_STATUS_CODE_AWARDED);
				award.setAwardStatus(awardDao.fetchAwardStatusByCode(Constants.AWARD_STATUS_CODE_AWARDED));
				award.setDocumentUpdateUser(award.getUpdateUser());
				award.setDocumentUpdateTimeStamp(commonDao.getCurrentTimestamp());
				awardDao.saveOrUpdateAwardDetails(award);
				if (!award.getAwardDocumentTypeCode().equals(Constants.MASTER_AWARD)) {
					AwardVO awardVO = new AwardVO();
					awardVO.setAward(award);
					String awardId = award.getAwardId().toString();
					Workflow workflow = workflowDao.fetchActiveWorkflowByParams(awardId, Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
					if (workflow != null) {
						awardVO.setWorkflow(workflow);
						List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(awardId, Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
						if (workFlows != null && !workFlows.isEmpty()) {
							awardVO.setWorkflowList(workFlows);
						}
					}
					businessRuleService.sendFinalApprovalNotification(awardVO);
				}
			}
		}
	}

	@Scheduled(cron = "${award.sentAwardReportRemainders.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void sentAwardReportRemainders() {
		Timestamp currentDate = commonDao.getCurrentTimestamp();
		logger.info("Award Remainder Scheduler Starts at " + currentDate);
		awardService.sentAwardRemainders();
	}

	@Scheduled(cron = "${award.workflowRemainder.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void sendRemainderEmailNotificationForAward() {
		logger.info("--------- sendRemainderEmailNotificationForAward ---------");
		Timestamp currentTime = commonDao.getCurrentTimestamp();
		List<Award> awards = awardDao.fetchAwardByStatusCode(Constants.AWARD_WORKFLOW_STATUS_APPROVAL_INPROGRESS);
		for (Award awardObject : awards) {
			if (awardObject.getSubmissionDate() != null) {
				long milliseconds = currentTime.getTime() - awardObject.getSubmissionDate().getTime();
				int seconds = (int) milliseconds / 1000;
				int hours = seconds / 3600;
				Workflow workflow = workflowDao.fetchActiveWorkflowByParams(awardObject.getAwardId().toString(), Constants.AWARD_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AWARD_SUBMODULE_CODE);
				if (hours > 24 && workflow != null) {
					List<WorkflowDetail> workflowDetails = workflow.getWorkflowDetails();
					for (WorkflowDetail workflowDetail : workflowDetails) {
						if (workflowDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
							Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<NotificationRecipient>();
							String approverPersonId = workflowDetail.getApproverPersonId();
							if (approverPersonId != null) {
								commonService.setNotificationRecipients(approverPersonId, "TO", dynamicEmailRecipients);
							}
							AwardVO awardVO = new AwardVO();
							awardVO.setAward(awardObject);
							awardService.sendAwardNotification(awardVO, Constants.REMINDER_NOTIFICATION_AWARD, dynamicEmailRecipients);
						}
					}
				}
			}
		}
	}
  
  @Scheduled(cron = "${award.updateAwardStatusExpired.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void updateAwardStatusToExpired() {
		logger.info("--------- updateAwardStatusToExpired ---------");
		List<Integer> expiredAwardIds = awardDao.fetchAllExpiringAwardIds();
		if (!expiredAwardIds.isEmpty()) {
			awardDao.updateAwardStatusToExpired(expiredAwardIds);
		}
	}

}
