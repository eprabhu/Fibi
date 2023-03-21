package com.polus.fibicomp.grantcall.vo;

import java.util.List;

import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.grantcall.pojo.GrantCallIOIHeader;
import com.polus.fibicomp.grantcall.pojo.GrantCallIOIMembers;
import com.polus.fibicomp.grantcall.pojo.GrantCallIOIQuestionnaire;
import com.polus.fibicomp.grantcall.pojo.GrantIOIStatus;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.questionnaire.dto.QuestionnaireDataBus;

public class GrantCallIOIVO {

	private Integer grantCallIOIId;

	private Integer grantCallId;

	private GrantCallIOIHeader grantCallIOI;

	private GrantCall grantCall;

	private GrantCallIOIMembers grantCallIOIMembers;

	private GrantCallIOIQuestionnaire grantCallIOIQuestionnaire;

	private GrantIOIStatus grantIOIStatus;

	private List<ProposalPersonRole> proposalPersonRoles;

	private Integer grantIOIMemberId;

	private List<GrantCallIOIMembers> grantCallIOIMemberList;

	private List<GrantCallIOIHeader> grantCallIOIHeaders;

	private String personId; 

	private String updateUser;

	private String message;

	private Boolean isAnswered = false;

	private boolean isCompleted = false;

	private String tabName;
	
	private String documentHeading;

	public Integer getGrantCallIOIId() {
		return grantCallIOIId;
	}

	public void setGrantCallIOIId(Integer grantCallIOIId) {
		this.grantCallIOIId = grantCallIOIId;
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

	public GrantCallIOIHeader getGrantCallIOI() {
		return grantCallIOI;
	}

	public void setGrantCallIOI(GrantCallIOIHeader grantCallIOI) {
		this.grantCallIOI = grantCallIOI;
	}

	public GrantCall getGrantCall() {
		return grantCall;
	}

	public void setGrantCall(GrantCall grantCall) {
		this.grantCall = grantCall;
	}

	public GrantCallIOIMembers getGrantCallIOIMembers() {
		return grantCallIOIMembers;
	}

	public void setGrantCallIOIMembers(GrantCallIOIMembers grantCallIOIMembers) {
		this.grantCallIOIMembers = grantCallIOIMembers;
	}

	public GrantCallIOIQuestionnaire getGrantCallIOIQuestionnaire() {
		return grantCallIOIQuestionnaire;
	}

	public void setGrantCallIOIQuestionnaire(GrantCallIOIQuestionnaire grantCallIOIQuestionnaire) {
		this.grantCallIOIQuestionnaire = grantCallIOIQuestionnaire;
	}

	public GrantIOIStatus getGrantIOIStatus() {
		return grantIOIStatus;
	}

	public void setGrantIOIStatus(GrantIOIStatus grantIOIStatus) {
		this.grantIOIStatus = grantIOIStatus;
	}

	public List<ProposalPersonRole> getProposalPersonRoles() {
		return proposalPersonRoles;
	}

	public void setProposalPersonRoles(List<ProposalPersonRole> proposalPersonRoles) {
		this.proposalPersonRoles = proposalPersonRoles;
	}

	public Integer getGrantIOIMemberId() {
		return grantIOIMemberId;
	}

	public void setGrantIOIMemberId(Integer grantIOIMemberId) {
		this.grantIOIMemberId = grantIOIMemberId;
	}

	public List<GrantCallIOIMembers> getGrantCallIOIMemberList() {
		return grantCallIOIMemberList;
	}

	public void setGrantCallIOIMemberList(List<GrantCallIOIMembers> grantCallIOIMemberList) {
		this.grantCallIOIMemberList = grantCallIOIMemberList;
	}

	public List<GrantCallIOIHeader> getGrantCallIOIHeaders() {
		return grantCallIOIHeaders;
	}

	public void setGrantCallIOIHeaders(List<GrantCallIOIHeader> grantCallIOIHeaders) {
		this.grantCallIOIHeaders = grantCallIOIHeaders;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public Boolean getIsAnswered() {
		return isAnswered;
	}

	public void setIsAnswered(Boolean isAnswered) {
		this.isAnswered = isAnswered;
	}

	public boolean isCompleted() {
		return isCompleted;
	}

	public void setCompleted(boolean isCompleted) {
		this.isCompleted = isCompleted;
	}

	public String getTabName() {
		return tabName;
	}

	public void setTabName(String tabName) {
		this.tabName = tabName;
	}

	public String getDocumentHeading() {
		return documentHeading;
	}

	public void setDocumentHeading(String documentHeading) {
		this.documentHeading = documentHeading;
	}

}
