package com.polus.fibicomp.notification.render;

import java.util.Map;

import org.apache.commons.collections4.map.HashedMap;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.evaluation.pojo.ProposalReview;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;

@Service(value = "proposalNotificationRenderService")
@PropertySource("classpath:application.properties")
public class ProposalNotificationRenderServiceImpl implements ProposalNotificationRenderService {

	@Value("${spring.application.name}")
	private String applicationURL;
	
	@Autowired
	private CommonService commonService;

	@Autowired
	private PersonDao personDao;

	@Override
	public Map<String, String> getDefaultReplacementParameters(Proposal proposal) {
		Map<String, String> result = new HashedMap<>();
		result.put("{PROPOSAL_NUMBER}", proposal.getProposalId() + "");
		result.put("{PROPOSAL_TITLE}", proposal.getTitle().replaceAll("\\s+", " "));
		result.put("{PRINCIPAL INVESTIGATOR}", proposal.getInvestigator().getFullName());
		result.put("{DEADLINE_DATE}", commonService.convertDateFormatBasedOnTimeZone(proposal.getSubmissionDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
		result.put("{LEAD_UNIT}", proposal.getHomeUnitNumber());
		result.put("{LEAD_UNIT_NAME}", proposal.getHomeUnitName());
		result.put("{APPLICATION_URL}", applicationURL);
		return result;
	}

	@Override
	public Map<String, String> getReplacementParametersForGrantCallOpenScheduler(GrantCall grantCall) {
		Map<String, String> result = new HashedMap<>();
		if (grantCall.getSponsorFundingScheme() != null) {
			result.put("{FUNDING_AGENCY}", grantCall.getSponsorFundingScheme().getDescription() + "");
		} else {
			result.put("{FUNDING_AGENCY}", "");
		}
		result.put("{GRANT_CALL_NAME}", grantCall.getGrantCallName().replaceAll("\\s+", " "));
		result.put("{GRANT_CALL_OPENING_DATE}", commonService.convertDateFormatBasedOnTimeZone(grantCall.getOpeningDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
		result.put("{GRANT_CALL_CLOSING_DATE}", commonService.convertDateFormatBasedOnTimeZone(grantCall.getClosingDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
		if (grantCall.getGrantTheme() != null) {
			result.put("{GRANT_CALL_THEME}", grantCall.getGrantTheme() + "");
		} else {
			result.put("{GRANT_CALL_THEME}", "");
		}
		if (grantCall.getOtherInformation() != null) {
			result.put("{OTHER_INFORMATION}", grantCall.getOtherInformation() + "");
		} else {
			result.put("{OTHER_INFORMATION}", "");
		}
		result.put("{APPLICATION_URL}", applicationURL);
		result.put("{GRANT_CALL_ID}", grantCall.getGrantCallId() + "");
		return result;
	}

	@Override
	public Map<String, String> getReplacementParametersForGrantCallOpen(GrantCall grantCall) {
		Map<String, String> result = new HashedMap<>();
		if (grantCall.getSponsorFundingScheme() != null) {
			result.put("{FUNDING_AGENCY}", grantCall.getSponsorFundingScheme().getDescription() + "");
		} else {
			result.put("{FUNDING_AGENCY}", "");
		}
		result.put("{GRANT_CALL_NAME}", grantCall.getGrantCallName().replaceAll("\\s+", " "));
		result.put("{GRANT_CALL_OPEN_DATE}",commonService.convertDateFormatBasedOnTimeZone(grantCall.getOpeningDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
		result.put("{GRANT_CALL_CLOSING_DATE}", commonService.convertDateFormatBasedOnTimeZone(grantCall.getClosingDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
		if (grantCall.getMaximumBudget() != null) {
			result.put("{MAX_BUDGET}", grantCall.getMaximumBudget().toString());
		} else {
			result.put("{MAX_BUDGET}", "");
		}
		if (grantCall.getOtherInformation() != null) {
			result.put("{OTHER_INFORMATION}", grantCall.getOtherInformation());
		} else {
			result.put("{OTHER_INFORMATION}", "");
		}
		result.put("{APPLICATION_URL}", applicationURL);
		result.put("{GRANT_CALL_ID}", grantCall.getGrantCallId() + "");
		if (grantCall.getGrantTheme() != null) {
			result.put("{GRANT_CALL_THEME}", grantCall.getGrantTheme() + "");
		} else {
			result.put("{GRANT_CALL_THEME}", "");
		}
		return result;
	}

	@Override
	public Map<String, String> getReplacementParametersForGrantCallClosing(GrantCall grantCall) {
		Map<String, String> result = new HashedMap<>();
		if (grantCall.getSponsorFundingScheme() != null) {
			result.put("{FUNDING_AGENCY}", grantCall.getSponsorFundingScheme().getDescription() + "");
		} else {
			result.put("{FUNDING_AGENCY}", "");
		}
		result.put("{GRANT_CALL_NAME}", grantCall.getGrantCallName().replaceAll("\\s+", " "));
		result.put("{GRANT_CALL_CLOSING_DATE}", commonService.convertDateFormatBasedOnTimeZone(grantCall.getClosingDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
		result.put("{APPLICATION_URL}", applicationURL);
		result.put("{GRANT_CALL_ID}", grantCall.getGrantCallId() + "");
		return result;
	}

	@Override
	public Map<String, String> getReplacementParametersForApplicationReview(String grantCallName,
			int numberOfApplications, String reviewerRole, ProposalReview proposalReview) {
		Map<String, String> result = new HashedMap<>();
		result.put("{NUMBER_OF_APPLICATIONS}", numberOfApplications + "");
		result.put("{GRANT_CALL_NAME}", grantCallName + "");
		result.put("{APPLICATION_URL}", applicationURL);
		if (proposalReview.getReviewDeadLineDate() != null) {
			result.put("{DEADLINE_DATE}", "by" + commonService.convertDateFormatBasedOnTimeZone(proposalReview.getReviewDeadLineDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
		} else {
			result.put("{DEADLINE_DATE}", "");
		}
		result.put("{REVIEWER_ROLE}", reviewerRole);
		return result;
	}

	@Override
	public Map<String, String> getReplacementParametersForApplicationReviewReminder(Proposal proposal,
			String reviewerRole) {
		Map<String, String> result = new HashedMap<>();
		if (proposal.getGrantCallName() != null) {
			result.put("{GRANT_CALL_NAME}", proposal.getGrantCallName() + "");
		} else {
			result.put("{GRANT_CALL_NAME}", "");
		}
		result.put("{DEADLINE_DATE}", commonService.convertDateFormatBasedOnTimeZone(proposal.getSponsorDeadlineDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
		result.put("{APPLICATION_URL}", applicationURL);
		result.put("{PROPOSAL_NUMBER}", proposal.getProposalId() + "");
		result.put("{REVIEWER_ROLE}", reviewerRole);
		return result;
	}

	@Override
	public Map<String, String> getReplacementParametersForCompletedReview(Proposal proposal, String reviewerRole) {
		Map<String, String> result = new HashedMap<>();
		result.put("{REVIEWER_ROLE}", reviewerRole);
		result.put("{TITLE}", proposal.getTitle() + "");
		result.put("{PRINCIPAL_INVESTIGATOR}", proposal.getInvestigator().getFullName() + "");
		if (proposal.getGrantCallName() != null) {
			result.put("{GRANT_CALL_NAME}", proposal.getGrantCallName() + "");
		} else {
			result.put("{GRANT_CALL_NAME}", "");
		}
		result.put("{TITLE}", proposal.getTitle());
		result.put("{LEAD_UNIT}", proposal.getHomeUnitName());
		result.put("{APPLICATION_URL}", applicationURL);
		result.put("{PROPOSAL_NUMBER}", proposal.getProposalId() + "");
		return result;
	}

	@Override
	public Map<String, String> getReplacementParametersForApplicationRevision(Proposal proposal,
			ProposalReview proposalReview) {
		Map<String, String> result = new HashedMap<>();
		result.put("{PRINCIPAL_INVESTIGATOR}", proposal.getInvestigator().getFullName());
		if (proposal.getGrantCallName() != null) {
			result.put("{GRANT_CALL_NAME}", proposal.getGrantCallName() + "");
		} else {
			result.put("{GRANT_CALL_NAME}", "");
		}
		result.put("{PI_NAME}", proposal.getInvestigator().getFullName() + "");
		result.put("{APPLICATION_ID}", proposal.getApplicationId() + "");
		result.put("{TITLE}", proposal.getTitle());
		result.put("{APPLICATION_URL}", applicationURL);
		result.put("{PROPOSAL_NUMBER}", proposal.getProposalId() + "");
		if (proposalReview.getReviewDeadLineDate() != null) {
			result.put("{DEADLINE_DATE}", "by " + commonService.convertDateFormatBasedOnTimeZone(proposalReview.getReviewDeadLineDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
		} else {
			result.put("{DEADLINE_DATE}", "");
		}
		return result;
	}

	@Override
	public Map<String, String> getParametersForApplicationRevisionReminder(Proposal proposal) {
		Map<String, String> result = new HashedMap<>();
		if (proposal.getGrantCallName() != null) {
			result.put("{GRANT_CALL_NAME}", proposal.getGrantCallName() + "");
		} else {
			result.put("{GRANT_CALL_NAME}", "");
		}
		result.put("{PRINCIPAL_INVESTIGATOR}", proposal.getInvestigator().getFullName() + "");
		if (proposal.getInternalDeadLineDate() != null) {
			result.put("{DEADLINE_DATE}", "by " + commonService.convertDateFormatBasedOnTimeZone(proposal.getSponsorDeadlineDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
		} else {
			result.put("{DEADLINE_DATE}", "");
		}
		result.put("{APPLICATION_URL}", applicationURL);
		result.put("{PROPOSAL_NUMBER}", proposal.getProposalId() + "");
		return result;
	}

	@Override
	public Map<String, String> getParametersForIRBAssessment(String grantCallName, int numberOfApplications,
			String reviewerRole, ProposalReview proposalReview) {
		Map<String, String> result = new HashedMap<>();
		result.put("{GRANT_CALL_NAME}", grantCallName + "");
		result.put("{NUMBER_OF_APPLICATIONS}", numberOfApplications + "");
		result.put("{APPLICATION_URL}", applicationURL);
		return result;
	}

	@Override
	public Map<String, String> getParametersForIRBAssessmentCompletion(Proposal proposal) {
		Map<String, String> result = new HashedMap<>();
		if (proposal.getGrantCallName() != null) {
			result.put("{GRANT_CALL_NAME}", proposal.getGrantCallName() + "");
		} else {
			result.put("{GRANT_CALL_NAME}", "");
		}
		result.put("{APPLICATION_URL}", applicationURL);
		result.put("{PROPOSAL_NUMBER}", proposal.getProposalId() + "");
		return result;
	}

	@Override
	public Map<String, String> getParametersForApplicationEndorsement(String grantCallName, int numberOfApplications,
			String reviewerRole, ProposalReview proposalReview) {
		Map<String, String> result = new HashedMap<>();
		result.put("{GRANT_CALL_NAME}", grantCallName + "");
		result.put("{NUMBER_OF_APPLICATIONS}", numberOfApplications + "");
		result.put("{REVIEWER_ROLE}", reviewerRole);
		result.put("{APPLICATION_URL}", applicationURL);
		return result;
	}

	public Map<String, String> getParametersForNotifyAction(String userName, String comment, String message,
			ServiceRequest serviceRequest) {
		Map<String, String> result = new HashedMap<>();
		result.put("{USER_NAME}", userName + "");
		result.put("{MESSAGE}", message + "");
		result.put("{SERVICE_REQUEST_ID}", serviceRequest.getServiceRequestId().toString());
		result.put("{SUMMARY}", serviceRequest.getSubject());
		result.put("{DESCRIPTION}", serviceRequest.getDescription());
		result.put("{COMMENT}", comment);
		result.put("{APPLICATION_URL}", applicationURL);
		return result;
	}

	@Override
	public Map<String, String> getParametersForServiceRequestAction(String message, ServiceRequest serviceRequest,
			String moduleItemKey, String userName, String editFieldsTableDetail) {

		Map<String, String> result = new HashedMap<>();
		if (editFieldsTableDetail != null) {
			result.put("{TABLE}", editFieldsTableDetail);
		} else {
			result.put("{TABLE}", "");
		}
		if (serviceRequest.getAssigneePersonId() != null) {
			result.put("{ASSIGNEE}", personDao.getPersonFullNameByPersonId(serviceRequest.getAssigneePersonId()));
		} else {
			result.put("{ASSIGNEE}", "");
		}
		if (userName != null) {
			result.put("{USER_NAME}", userName);
		} else {
			result.put("{USER_NAME}", "");
		}
		result.put("{SERVICE_REQUEST_ID}", serviceRequest.getServiceRequestId().toString());
		result.put("{REPORTER_USER_NAME}", personDao.getPersonFullNameByPersonId(serviceRequest.getReporterPersonId()));
		if (serviceRequest.getServiceRequestType() != null) {
			result.put("{SERVICE_TYPE}", serviceRequest.getServiceRequestType().getDescription() + "");
		} else {
			result.put("{SERVICE_TYPE}", "");
		}
		if (message != null) {
			result.put("{MESSAGE}", message + "");
		} else {
			result.put("{MESSAGE}", "");
		}
		if (moduleItemKey != null) {
			result.put("{MODULE_ITEM_KEY}", moduleItemKey);
		} else {
			result.put("{MODULE_ITEM_KEY}", "test");
		}
		if (serviceRequest.getSubject() != null) {
			result.put("{SUBJECT}", serviceRequest.getSubject());
		} else {
			result.put("{SUBJECT}", "");
		}
		if (serviceRequest.getUnitNumber() != null) {
			result.put("{DEPARTMENT}", serviceRequest.getUnit().getUnitName());
		} else {
			result.put("{DEPARTMENT}", "");
		}
		result.put("{APPLICATION_URL}", applicationURL+"/service-request/");
		return result;
	}

}
