package com.polus.fibicomp.proposal.schedule;

import java.sql.Timestamp;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.service.ProposalService;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;

@Component
@Transactional
public class proposalScheduler {

	protected static Logger logger = LogManager.getLogger(proposalScheduler.class.getName());

	@Autowired
	private WorkflowDao workflowDao;

	@Autowired
	ProposalDao proposalDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private ProposalService proposalService;

	@Scheduled(cron = "${proposal.remainder.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void sendRemainderEmailNotificationForProposal() {
		logger.info("--------- sendRemainderEmailNotificationForProposal ---------");
		Date date = new Date();
		Timestamp currentTime = new Timestamp(date.getTime());
		List<Proposal> proposals = proposalDao.fetchProposalsByStatusCode(Constants.PROPOSAL_STATUS_CODE_APPROVAL_INPROGRESS);
		for (Proposal proposalObject : proposals) {
			if (proposalObject.getSubmissionDate() != null) {
				long milliseconds = currentTime.getTime() - proposalObject.getSubmissionDate().getTime();
				int seconds = (int) milliseconds / 1000;
				int hours = seconds / 3600;
				Workflow workflow = workflowDao.fetchActiveWorkflowByParams(proposalObject.getProposalId().toString(), Constants.DEV_PROPOSAL_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.DEV_PROPOSAL_SUBMODULE_CODE);
				if (hours > 24 && workflow != null) {
					List<WorkflowDetail> workflowDetails = workflow.getWorkflowDetails();
					for (WorkflowDetail workflowDetail : workflowDetails) {
						if (workflowDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
							Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<NotificationRecipient>();
							String approverPersonId = workflowDetail.getApproverPersonId();
							if (approverPersonId != null) {
								commonService.setNotificationRecipients(approverPersonId, "TO", dynamicEmailRecipients);
							}
							ProposalVO proposalVO = new ProposalVO();
							proposalVO.setProposal(proposalObject);
							proposalService.sendProposalNotification(proposalVO, Constants.REMINDER_NOTIFICATION_PROPOSAL, dynamicEmailRecipients);
						}
					}
				}
			}
		}
	}

}