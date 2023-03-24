package com.polus.fibicomp.notification.render;

import java.util.Map;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.evaluation.pojo.ProposalReview;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;

@Service
public interface ProposalNotificationRenderService {

	/**
	 * This method is used to fetch replacement parameters for submit proposal.
	 * @param proposal.
	 * @return replacement parameters data.
	 */
	public Map<String, String> getDefaultReplacementParameters(Proposal proposal);

	/**
	 * This method is used to fetch replacement parameters for grant call open scheduler.
	 * @param grantCall.
	 * @return replacement parameters data.
	 */
	public Map<String, String> getReplacementParametersForGrantCallOpenScheduler(GrantCall grantCall);

	/**
	 * This method is used to fetch replacement parameters for grant call open.
	 * @param grantCall.
	 * @return replacement parameters data.
	 */
	public Map<String, String> getReplacementParametersForGrantCallOpen(GrantCall grantCall);

	/**
	 * This method is used to fetch replacement parameters for grant call closing.
	 * @param grantCall.
	 * @return replacement parameters data.
	 */
	public Map<String, String> getReplacementParametersForGrantCallClosing(GrantCall grantCall);

	/**
	 * This method is used to fetch replacement parameters for application review.
	 * @param grantCallName.
	 * @param numberOfApplications.
	 * @param reviewerRole.
	 * @return replacement parameters data.
	 */
	public Map<String, String> getReplacementParametersForApplicationReview(String grantCallName, int numberOfApplications, String reviewerRole, ProposalReview proposalReview);

	/**
	 * This method is used to fetch replacement parameters for application review reminder.
	 * @param proposal.
	 * @param reviewerRole.
	 * @return replacement parameters data.
	 */
	public Map<String, String> getReplacementParametersForApplicationReviewReminder(Proposal proposal, String reviewerRole);

	/**
	 * This method is used to fetch replacement parameters for complete review.
	 * @param proposal.
	 * @param reviewerRole.
	 * @return replacement parameters data.
	 */
	public Map<String, String> getReplacementParametersForCompletedReview(Proposal proposal, String reviewerRole);

	/**
	 * This method is used to fetch replacement parameters for application revision.
	 * @param proposal.
	 * @return replacement parameters data.
	 */
	public Map<String, String> getReplacementParametersForApplicationRevision(Proposal proposal, ProposalReview proposalReview);

	/**
	 * This method is used to fetch replacement parameters for application revision reminder.
	 * @param proposal.
	 * @return replacement parameters data.
	 */
	public Map<String, String> getParametersForApplicationRevisionReminder(Proposal proposal);

	/**
	 * This method is used to fetch replacement parameters for IRB Assessment.
	 * @param proposal.
	 * @param numberOfApplications
	 * @return replacement parameters data.
	 */
	public Map<String, String> getParametersForIRBAssessment(String grantCallName, int numberOfApplications, String reviewerRole, ProposalReview proposalReview);

	/**
	 * This method is used to fetch replacement parameters for IRB Assessment completion.
	 * @param proposal.
	 * @return replacement parameters data.
	 */
	public Map<String, String> getParametersForIRBAssessmentCompletion(Proposal proposal);

	/**
	 * This method is used to fetch replacement parameters for application endorsement.
	 * @param grantCallName.
	 * @param numberOfApplications.
	 * @param reviewerRole
	 * @return replacement parameters data.
	 */
	public Map<String, String> getParametersForApplicationEndorsement(String grantCallName, int numberOfApplications, String reviewerRole, ProposalReview proposalReview);

	/**
	 * This method is used to fetch replacement parameters for ntify action.
	 * @param userName.
	 * @param comment.
	 * @param message
	 * @param serviceRequest
	 * @return replacement parameters data.
	 */
	public Map<String, String> getParametersForNotifyAction(String userName, String comment, String message, ServiceRequest serviceRequest);

	/**
	 * This method is used to fetch replacement parameters for service request action.
	 * @param message
	 * @param serviceRequest
	 * @param accountNumber
	 * @param userName
	 * @param editFieldsTableDetail
	 * @return replacement parameters data.
	 */
	public Map<String, String> getParametersForServiceRequestAction(String message, ServiceRequest serviceRequest, String moduleItemKey, String userName, String editFieldsTableDetail);

}
