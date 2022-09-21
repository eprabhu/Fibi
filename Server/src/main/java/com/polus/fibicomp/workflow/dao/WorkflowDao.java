package com.polus.fibicomp.workflow.dao;

import java.util.List;
import java.util.Set;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.person.pojo.PersonRoleType;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowAttachment;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;
import com.polus.fibicomp.workflow.pojo.WorkflowDetailExt;
import com.polus.fibicomp.workflow.pojo.WorkflowFeedbackType;
import com.polus.fibicomp.workflow.pojo.WorkflowMapDetail;
import com.polus.fibicomp.workflow.pojo.WorkflowMapType;
import com.polus.fibicomp.workflow.pojo.WorkflowStatus;

@Transactional
@Service
public interface WorkflowDao {

	/**
	 * This method is used to save workflow.
	 * @param workflow - workflow object.
	 * @return an object of workflow.
	 */
	public Workflow saveWorkflow(Workflow workflow);

	/**
	 * This method is used to fetch workflow map details.
	 * @return a list of workflow map detail.
	 */
	public List<WorkflowMapDetail> fetchWorkflowMapDetail();

	/**
	 * This method is used to fetch workflow status based on workflow status code.
	 * @param approveStatusCode - workflow status code.
	 * @return an object of workflow status.
	 */
	public WorkflowStatus fetchWorkflowStatusByStatusCode(String approveStatusCode);

	/**
	 * This method is used to fetch active workflow based on module item Id, moduleCode ,subModuleCode, subModuleItemId.
	 * @param moduleItemId - Id of the module.
	 * @param subModuleItemId - Id of the subModule.
	 * @param moduleCode 
	 * @param subModuleCode 
	 * @return an object of workflow.
	 */
	public Workflow fetchActiveWorkflowByParams(String moduleItemId, Integer moduleCode, String subModuleItemId, Integer subModuleCode);

	/**
	 * This method is used to fetch workflow detail.
	 * @param workflowId - Id of the workflow.
	 * @param personId - Person Id of the logged in user.
	 * @param approverStopNumber - Stop Number of the approver.
	 * @return an object of workflow detail.
	 */
	public WorkflowDetail findUniqueWorkflowDetailByCriteria(Integer workflowId, String personId,boolean isSuperUser, Integer approverStopNumber);

	/**
	 * This method is used to save workflow detail.
	 * @param workflowDetail - workflow detail object.
	 * @return an object of workflow detail.
	 */
	public WorkflowDetail saveWorkflowDetail(WorkflowDetail workflowDetail);

	/**
	 * This method is used to fetch workflow attachment based on attachment Id.
	 * @param attachmentId - Id of the workflow attachment.
	 * @return an object of workflow attachment.
	 */
	public WorkflowAttachment fetchWorkflowAttachmentById(Integer attachmentId);

	/**
	 * This method is used to fetch workflow details based on stop number.
	 * @param workflowId - Id of the workflow.
	 * @param approvalStopNumber - Stop Number of the approver.
	 * @param approvalStatusCode - status code of workflow detail.
	 * @return a list of workflow detail.
	 */
	public List<WorkflowDetail> fetchWorkflowDetailListByApprovalStopNumber(Integer workflowId, Integer approvalStopNumber, String approvalStatusCode);

	/**
	 * This method is used to fetch the maximum stop number.
	 * @param workflowId - Id of the workflow.
	 * @return the max value of stop number.
	 */
	public Integer getMaxStopNumber(Integer workflowId);

	/**
	 * This method is used to fetch workflow detail based on input params.
	 * @param workflowId - Id of the workflow.
	 * @param personId - Person Id of the logged in user.
	 * @param stopNumber - Stop Number of the approver.
	 * @return an object of workflow detail.
	 */
	public WorkflowDetail fetchWorkflowByParams(Integer workflowId, String personId, Integer stopNumber);

	/**
	 * This method is used to fetch workflow detail based on id.
	 * @param workflowId - Id of the workflow.
	 * @return an object of workflow detail.
	 */
	public WorkflowDetail fetchWorkflowDetailById(Integer workflowId);

	/**
	 * This method is used to fetch the count of active workflow.
	 * @param moduleItemId - Id of the module.
	 * @return count of workflow.
	 */
	public Long activeWorkflowCountByModuleItemId(String moduleItemId);

	/**
	 * This method is used to fetch current workflow detail.
	 * @param workflowId - Id of the workflow.
	 * @param personId - Person Id of the logged in user.
	 * @param roleCode - role code of the user.
	 * @return an object of workflow detail.
	 */
	public WorkflowDetail getCurrentWorkflowDetail(Integer workflowId, String personId, Integer roleCode);

	/**
	 * This method is used to fetch workflow details.
	 * @param workflowId - Id of the workflow.
	 * @return a list of workflow details.
	 */
	public List<WorkflowDetail> fetchWorkflowDetailByWorkflowId(Integer workflowId);

	/**
	 * This method is used to get mail address based on user type.
	 * @param roleTypeCode - specifies the type of user in routing
	 * @return set of email address of routing users.
	 */
	public Set<String> fetchEmailAdressByUserType(String roleTypeCode);

	/**
	 * This method is used to fetch workflow map details based on role type.
	 * @param roleTypeCode - specifies the type of user in routing
	 * @return a list of workflow map details.
	 */
	public List<WorkflowMapDetail> fetchWorkflowMapDetailByRoleType(Integer roleTypeCode);

	public List<WorkflowDetail> fetchFinalApprover(Integer workflowId, Integer approvalStopNumber);

	/**
	 * This method is used to fetch all workflows based on module item Id, moduleCode ,subModuleCode, subModuleItemId.
	 * @param moduleItemId - Id of the module.
	 * @param subModuleItemId - Id of the module.
	 * @param subModuleCode 
	 * @return an object of workflow.
	 */
	public List<Workflow> fetchWorkflowsByParams(String moduleItemId, Integer moduleCode, String subModuleItemId, Integer subModuleCode);

	public Integer getWaitingForApprovalStopNumber(String statusCode, Integer workflowId);

	public Person getPersonDetail(String personId);

	public PersonRoleType getWorkflowRoleTypeById(Integer roleTypeId);

	public Integer getMaxApproverNumber(Integer workFlowId, Integer mapId, Integer mapNumber);
    
	public List<WorkflowMapType> fetchAllWorkflowMapTypes();

	/**
	 * This method is used to fetch workflow detail based on user role.
	 * @param workflowId - workflowId.
	 * @param roleTypeCode - user role code
	 * @return a list of workflow details.
	 */
	public List<WorkflowDetail> getWorkflowDetailByUserRole(Integer workflowId, Integer roleTypeCode);

	/**
	 * This method is used to fetch stopName based on Map Id and approvalStopNumber.
	 * @param mapId - mapId.
	 * @param approvalStopNumber -approvalStopNumber
	 * @return a String value of stopName.
	 */
	public String fetchStopNameBasedMapIdAndStop(Integer mapId, Integer approvalStopNumber);

	/**
	 * This method is used to fetch workflow detail based on params.
	 * @param workflowId - workflowId.
	 * @param workFlowStatusCode - workFlowStatusCode
	 * @return workflow detail.
	 */
	public WorkflowDetail getWorkflowDetailBasedOnParams(Integer workflowId, String workFlowStatusCode);

	/**
	 * This method is used to fetch the proposal name based on Id.
	 * @param proposalId - Id of the proposal.
	 * @param personId - Id of the person.
	 * @return mapId.
	 */
	public Integer fetchWorkflowDetailId(Integer proposalId, String personId);

	/**
	 * This method is used to select the next stop number based on workflow id and map id
	 * @param mapId
	 * @param workFlowId
	 * @return next stop number 
	 */
	public Integer getNextStopNumberBasedOnMap(Integer mapId, Integer workFlowId);

	/**
	 * This method is used to select person Ids based on moduleItemId and moduleCode
	 * @param moduleItemId
	 * @param moduleCode
	 * @return list of personId
	 */
	public List<String> getLatestWorkflowPersonIdsByParams(String moduleItemId, Integer moduleCode);

	/**
	 * This method is used to get the workflow latest workflow.
	 * @param workflowId
	 * @param moduleItemId
	 * @param moduleCode
	 * @param subModuleItemId
	 * @param subModuleCode
	 */
	public Workflow getActiveWorkFlow(String moduleItemId, Integer moduleCode, String subModuleItemId, Integer subModuleCode);

	/**
	 * This method is used to get the details of scoring workflow details
	 * @param proposalId
	 * @return list of workflow details who can score
	 */
	public List<WorkflowDetail> getScoringWorkflowDetails(Integer proposalId);

	/**
	 * @param moduleCode
	 * @param subModuleCode
	 * @return List of WorkflowFeedbackType
	 */
	public List<WorkflowFeedbackType> fetchWorkflowFeedbackTypeBasedOnParam(Integer moduleCode, Integer subModuleCode);

	/**
	 * @param feedbackTypeCode
	 * @return object of WorkflowFeedbackType
	 */
	public WorkflowFeedbackType fetchWorkflowFeedbackTypeBasedOnCode(String feedbackTypeCode);

	/**
	 * @param workflowDetailId
	 * @return object Of WorkflowDetailExt 
	 */
	public WorkflowDetailExt fetchWorkflowExtBasedOnWorkflowDetailId(Integer workflowDetailId);

	/**
	 * @param workflowDetailExt
	 */
	public void saveOrUpdateWorkflowDetailExt(WorkflowDetailExt workflowDetailExt);

	/**
	 * @param workflowdetailId
	 * @return object Of WorkflowDetail 
	 */
	public WorkflowDetail getWorkFlowDetails(Integer workflowdetailId);
}
