package com.polus.fibicomp.agreements.service;

import java.util.Collections;
import java.util.List;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.agreements.vo.AgreementVO;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.workflow.comparator.WorkflowDetailComparator;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;

@Transactional
@Service(value ="agreementWorkflowService")
public class AgreementWorkflowServiceImpl implements AgreementWorkflowService{

	@Autowired
	private WorkflowDao workflowDao;

	@Override
	public void canTakeRoutingAction(AgreementVO agreementVO) {
		AgreementHeader agreement = agreementVO.getAgreementHeader();
		String agreementId = agreement.getAgreementRequestId().toString();
		Workflow workflow = agreementVO.getWorkflow();
		if (workflow == null) {
			workflow = workflowDao.fetchActiveWorkflowByParams(agreementId, Constants.AGREEMENT_MODULE_CODE, Constants.SUBMODULE_ITEM_KEY, Constants.AGREEMENT_SUBMODULE_CODE);
		}
		if (workflow != null) {
			Integer maxApprovalStopNumber = workflowDao.getMaxStopNumber(workflow.getWorkflowId());
			List<WorkflowDetail> finalWorkflowDetails = workflowDao.fetchFinalApprover(workflow.getWorkflowId(), maxApprovalStopNumber);
			if (finalWorkflowDetails != null && !finalWorkflowDetails.isEmpty()) {
				for (WorkflowDetail finalWorkflowDetail : finalWorkflowDetails) {
					if (finalWorkflowDetail.getApproverPersonId().equals(agreementVO.getPersonId())
							|| finalWorkflowDetail.getApprovalStopNumber().equals(maxApprovalStopNumber)) {
						agreementVO.setFinalApprover(true);
					}
				}
			}
			List<WorkflowDetail> workflowDetails = workflow.getWorkflowDetails();
			if (workflowDetails != null && !workflowDetails.isEmpty()) {
				Collections.sort(workflowDetails, new WorkflowDetailComparator());
				boolean currentPerson = true;
				if (agreement.getAgreementStatusCode().equals("1")) {
					for (WorkflowDetail workflowDetail : workflowDetails) {
						if (currentPerson == true) {
							if (workflowDetail.getApproverPersonId().equals(agreementVO.getPersonId())) {
								if (agreement.getAgreementStatusCode().equals("1")&& workflowDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
									if (workflowDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_APPROVED)) {
										agreementVO.setIsApproved(true);
									} else {
										agreementVO.setIsApproved(false);
									}
									agreementVO.setIsApprover(true);
								}
							}
						}
					}
				}
			}
		}
	}
}
