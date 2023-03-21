package com.polus.fibicomp.workflow.service;

import java.util.List;
import java.util.Set;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;

@Service
public interface WorkflowService {

	/**
	 * This method is used to download Workflow Attachment.
	 * @param attachmentId - Id of the attachment to download.
	 * @return attachmentData.
	 */
	public ResponseEntity<byte[]> downloadWorkflowAttachment(Integer attachmentId);

	/**
	 * This method is used to fetch current workflow detail.
	 * @param workflowId - Id of the workflow.
	 * @param personId - Person Id of the logged in user.
	 * @param roleCode - role code of the user.
	 * @return an object of workflow detail.
	 */
	public WorkflowDetail getCurrentWorkflowDetail(Integer workflowId, String personId, Integer roleCode);

	/**
	 * This method is used to get mail address based on user type.
	 * @param roleType - specifies the type of user in routing
	 * @return set of email address of routing users.
	 */
	public Set<String> getEmailAdressByUserType(String roleTypeCode);

	/**
	 * This method is used to prepare workflow.
	 */
	public void prepareWorkflowDetails(Workflow workflow);

	/**
	 * This method is used to prepare workflow list.
	 */
	public void prepareWorkflowDetailsList(List<Workflow> workflowList);

	public String fetchWorkflowMapType();
    
}
