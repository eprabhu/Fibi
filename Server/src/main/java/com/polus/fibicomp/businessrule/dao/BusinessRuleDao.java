package com.polus.fibicomp.businessrule.dao;

import java.util.HashMap;
import java.util.List;

import com.polus.fibicomp.businessrule.dto.WorkFlowResultDto;
import com.polus.fibicomp.businessrule.vo.EvaluateValidationRuleVO;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowAttachment;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;

public interface BusinessRuleDao {

	/**
	 * this method is used for evaluate workflow
	 * @param vO
	 * @return
	 */
	public Integer buildWorkFlow(EvaluateValidationRuleVO VO);

	/**
	 * this method is used for evaluate validation
	 * @param vO
	 * @return
	 */
	public List<WorkFlowResultDto> evaluateValidationRule(EvaluateValidationRuleVO VO);
	
	/**
	 * this method is used for evaluate notification
	 * @param VO
	 * @return
	 */
	public List<String> evaluateNotificationRule(EvaluateValidationRuleVO VO);

	/**
	 * this method is used for evaluate rule
	 * @param VO
	 * @return
	 */
	public String ruleEvaluate(EvaluateValidationRuleVO VO);

	/**
	 * this method is used for display the user whether he can approve or disapprove the proposal
	 * @param moduleItemKey
	 * @param loginPersonId
	 * @return
	 */
	public Integer canApproveRouting(String moduleItemKey, String loginPersonId, Integer moduleCode, String subModuleItemKey, Integer subModuleCode);

	/**
	 * this method is used for display the routing details with respect to the module_item_id 
	 * @param module_item_id
	 * @return
	 */
	public String getWorkFlowRouteLog(String module_item_id,Integer moduleCode);

	/**
	 * this method is used to check current login user is the final approver or not
	 * @param moduleItemKey
	 * @param personId
	 * @return
	 */
	public String workflowfinalApproval(String moduleItemKey, String personId, Integer moduleCode, String subModuleItemKey, Integer subModuleCode);

	/**
	 * this method is used for save attachments in workflow
	 * @param workflowAttachments
	 * @return
	 */
	public WorkflowAttachment saveWorkflowAttachment(WorkflowAttachment workflowAttachments);

	/**
	 * this method is used for get email id of a person by his name
	 * @param negotiatorName
	 * @return
	 */
	public String getEmailIdByName(String negotiatorId);

	/**
	 * 
	 * @param workflowDetail
	 * @return
	 */
	public String savOrUpdateWorkflowDetails(WorkflowDetail workflowDetail);

	/**
	 * this method is used to get workflow table values 
	 * @param moduleCode - used to check it is proposals or negotiations or award
	 * @param moduleItemKey - negotiation id in case negotiation ,proposal number in case of proposal
	 * @return
	 */
	public List<Workflow> getWorkflow(String moduleCode, String moduleItemKey);

	/**
	 * 
	 * @param moduleItemKey
	 * @param moduleCode
	 * @return
	 */
	public Workflow getWorkFlow(String moduleItemKey, Integer moduleCode);

	/**
	 * 
	 * @param integer3 
	 * @param string 
	 * @param integer2 
	 * @param integer 
	 * @param VO
	 * @return
	 */
	public String workflowApprove(String moduleItemKey, String moduleCode, String logginPersonId, String updatedUser,
			String actionType, String approverComment, Integer subModuleCode, String subModuleItemKey, Integer mapId, Integer mapNumber, Integer approverStopNumber, Integer approverNumber);

	/**
	 * 
	 * @param workflowAttachmentId
	 * @return
	 */
	public WorkflowAttachment fetchAttachmentById(Integer workflowAttachmentId);

	/**
	 * this method is used for fetch mapName and Role name when role based person are not assigned in workflow
	 * @param moduleItemKey
	 * @param moduleCode
	 * @param subModuleCode
	 * @param subModuleItemKey
	 * @return
	 */
	public List<HashMap<String, Object>> getRoleNameAndMapName(Integer moduleCode, String moduleItemKey, Integer subModuleCode, String subModuleItemKey);

}
