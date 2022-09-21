package com.polus.fibicomp.grantcall.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.Unit;

@Entity
@Table(name = "GRANT_CALL_IOI_HEADER")
public class GrantCallIOIHeader implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "IOI_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GRANT_CALL_IOI_ID_GENERATOR")
	@SequenceGenerator(name="GRANT_CALL_IOI_ID_GENERATOR", sequenceName = "GRANT_CALL_IOI_ID_GENERATOR", allocationSize=1)
	private Integer grantCallIOIId;

	@Column(name = "GRANT_HEADER_ID")
	private Integer grantCallId;

	@Column(name = "PROJECT_TITLE")
	private String projectTitle;

	@Column(name = "REQUESTED_DIRECT_COST")
	private BigDecimal requestedDirectCost;

	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@Column(name = "PI_PERSON_ID")
	private String principalInvestigatorId;

	@Column(name = "SUBMITING_UNIT_NUMBER")
	private String submittingUnitNumber;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "STATUS_CODE")
	private Integer grantIOIStatusCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_IOI_HEADER_FK2"), name = "STATUS_CODE", referencedColumnName = "STATUS_CODE", insertable = false, updatable = false)
	private GrantIOIStatus grantIOIStatus;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_IOI_HEADER_FK3"), name = "UNIT_NUMBER", referencedColumnName = "UNIT_NUMBER", insertable = false, updatable = false)
	private Unit unitDetails;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_IOI_HEADER_FK4"), name = "PI_PERSON_ID", referencedColumnName = "PERSON_ID", insertable = false, updatable = false)
	private Person person;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_IOI_HEADER_FK5"), name = "SUBMITING_UNIT_NUMBER", referencedColumnName = "UNIT_NUMBER", insertable = false, updatable = false)
	private Unit submittingUnitDetails;

	@JsonManagedReference
	@OneToMany(mappedBy = "grantCallIOIHeader", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<GrantCallIOIMembers> grantCallIOIMembers;

	@Column(name = "QUESTIONNAIRE_ANSWER_ID")
	private Integer questionnaireAnswerId;

	@Transient
	private Integer memberCount;

	@Transient
	private String userFullName;

	@Transient
	private String piFullName;

	@Transient
	private String updateUserFullName;

	@Transient
	private String submittingUnitName;

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

	public String getProjectTitle() {
		return projectTitle;
	}

	public void setProjectTitle(String projectTitle) {
		this.projectTitle = projectTitle;
	}

	public BigDecimal getRequestedDirectCost() {
		return requestedDirectCost;
	}

	public void setRequestedDirectCost(BigDecimal requestedDirectCost) {
		this.requestedDirectCost = requestedDirectCost;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public String getPrincipalInvestigatorId() {
		return principalInvestigatorId;
	}

	public void setPrincipalInvestigatorId(String principalInvestigatorId) {
		this.principalInvestigatorId = principalInvestigatorId;
	}

	public String getSubmittingUnitNumber() {
		return submittingUnitNumber;
	}

	public void setSubmittingUnitNumber(String submittingUnitNumber) {
		this.submittingUnitNumber = submittingUnitNumber;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Integer getGrantIOIStatusCode() {
		return grantIOIStatusCode;
	}

	public void setGrantIOIStatusCode(Integer grantIOIStatusCode) {
		this.grantIOIStatusCode = grantIOIStatusCode;
	}

	public GrantIOIStatus getGrantIOIStatus() {
		return grantIOIStatus;
	}

	public void setGrantIOIStatus(GrantIOIStatus grantIOIStatus) {
		this.grantIOIStatus = grantIOIStatus;
	}

	public Unit getUnitDetails() {
		return unitDetails;
	}

	public void setUnitDetails(Unit unitDetails) {
		this.unitDetails = unitDetails;
	}

	public Person getPerson() {
		return person;
	}

	public void setPerson(Person person) {
		this.person = person;
	}

	public Unit getSubmittingUnitDetails() {
		return submittingUnitDetails;
	}

	public void setSubmittingUnitDetails(Unit submittingUnitDetails) {
		this.submittingUnitDetails = submittingUnitDetails;
	}

	public List<GrantCallIOIMembers> getGrantCallIOIMembers() {
		return grantCallIOIMembers;
	}

	public void setGrantCallIOIMembers(List<GrantCallIOIMembers> grantCallIOIMembers) {
		this.grantCallIOIMembers = grantCallIOIMembers;
	}

	public Integer getQuestionnaireAnswerId() {
		return questionnaireAnswerId;
	}

	public void setQuestionnaireAnswerId(Integer questionnaireAnswerId) {
		this.questionnaireAnswerId = questionnaireAnswerId;
	}

	public Integer getMemberCount() {
		return memberCount;
	}

	public void setMemberCount(Integer memberCount) {
		this.memberCount = memberCount;
	}

	public String getUserFullName() {
		return userFullName;
	}

	public void setUserFullName(String userFullName) {
		this.userFullName = userFullName;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public String getPiFullName() {
		return piFullName;
	}

	public void setPiFullName(String piFullName) {
		this.piFullName = piFullName;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public String getSubmittingUnitName() {
		return submittingUnitName;
	}

	public void setSubmittingUnitName(String submittingUnitName) {
		this.submittingUnitName = submittingUnitName;
	}

}
