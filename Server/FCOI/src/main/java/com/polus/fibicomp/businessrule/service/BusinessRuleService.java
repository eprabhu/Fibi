package com.polus.fibicomp.businessrule.service;

import java.util.Map;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.businessrule.vo.EvaluateValidationRuleVO;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.workflow.pojo.Workflow;

@Service
public interface BusinessRuleService {

	/**
	 * evaluateValidationRule workflow 
	 * @param vO
	 * @return
	 */
	public Integer buildWorkFlow(EvaluateValidationRuleVO vO);

	/**
	 * this method is used for evaluate validation rule
	 * @param evaluateValidationRuleVO
	 * @return
	 */
	public String evaluateValidationRule(EvaluateValidationRuleVO evaluateValidationRuleVO);

	/**
	 * this method is used for evaluate notification rule
	 * @param evaluateValidationRuleVO
	 * @return
	 */
	public String evaluateNotificationRule(EvaluateValidationRuleVO evaluateValidationRuleVO);

	/**
	 * this method is used for evaluate a rule based on input rule id and module data.
	 * @param evaluateValidationRuleVO
	 * @return
	 */
	public String ruleEvaluate(EvaluateValidationRuleVO evaluateValidationRuleVO);

	/**
	 * this method is used for display the user whether he can approve or disapprove the Routing
	 * @param moduleItemKey
	 * @param moduleCode
	 * @param subModuleCode
	 * @param subModuleItemKey
	 * @param loginPersonId
	 * @return
	 */
	public Integer canApproveRouting(String moduleItemKey, String loginPersonId, Integer moduleCode, String subModuleItemKey, Integer subModuleCode);

	/**
	 * this method is used to get workflow routing log with respect to the proposal id
	 * @param moduleItemId
	 * @param moduleCode
	 * @return
	 */
	public String getWorkFlowRouteLog(String moduleItemId,Integer moduleCode);

	/**
	 * this method is used to check current login user is the final approver or not
	 * @param moduleItemKey
	 * @param personId
	 * @return
	 */
	public String workflowfinalApproval(String moduleItemKey, String personId, Integer moduleCode, Integer subModuleCode, String subModuleItemKey);

	/**
	 * this method is used for the click actions for the button APPROVE or DISAPPROVE,
	 * person can add attachments also person can add comments with there action.
	 * action code : R is used to disapprove and action code A is used to approve 
	 * @param files
	 * @param formDataJson
	 * @param moduleCode
	 * @param subModuleCode
	 * @return
	 */
	public String approveOrRejectWorkflow(MultipartFile[] files, String formDataJson, String moduleCode, String subModuleCode);

	/**
	 * @param moduleItemKey
	 * @param moduleCode
	 * @return
	 */
	public Workflow getWorkFlow(String moduleItemKey, Integer moduleCode);

	/**
	 * 
	 * @param workflowAttachmentId
	 * @return
	 */
	public ResponseEntity<byte[]> downloadWorkflowsAttachments(Integer workflowAttachmentId);

	/**
	 * This method is used to generate IP 
	 * @param proposalVO
	 * @return proposal Object
	 */
	public Proposal generateInstitutionalProposal(ProposalVO proposalVO);

	/**
	 * This method is used for endorse action
	 * @param proposalId
	 * @param updatedUser
	 * @param moduleItemKey
	 * @param logginPersonId
	 * @param subModuleCode
	 * @return
	 */
	public ProposalVO endorseProposal(Integer proposalId, String updatedUser, String moduleItemKey,
			String logginPersonId, Integer subModuleCode);

	/**
	 * This method is used for sending notification in final approval of Award or service request
	 * @param awardVO - awardVO
	 */
	public void sendFinalApprovalNotification(AwardVO awardVO);

	/**
	 * This method is used to send notification based on the business rule configuration
	 * @param moduleCode
	 * @param subModuleCode
	 * @param moduleItemKey
	 * @param string
	 * @param personId
	 * @param updateUser
	 */
	public void evaluateAndSentNotification(Integer moduleCode, Integer subModuleCode, String moduleItemKey,
			String string, String personId, String updateUser, Map<String, String> placeHolder);

	public String approveOrRejectWorkflowForWaf(MultipartFile file, String formDataJson, String moduleCode);

	/**
	 * This method is used to approveOrRejectAwardWorkflow 
	 * @param awardVO
	 * @return String Object
	 */
	public AwardVO approveOrRejectAwardWorkflow(AwardVO awardVO);

	/**
	 * @param files
	 * @param formDataJson
	 * @param moduleCode
	 * @return Object of AwardVO
	 */
	public AwardVO awardWorkflowApproval(MultipartFile[] files, String formDataJson, String moduleCode);

	Boolean canCreateInstituteProposal(Proposal proposal, ProposalVO proposalVO);

	/**
	 * @param awardVO
	 * @return
	 */
	public String approveOrRejectAwardWorkflowForWaf(AwardVO awardVO);

}
