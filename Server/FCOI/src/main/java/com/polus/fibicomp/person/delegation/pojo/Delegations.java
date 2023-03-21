package com.polus.fibicomp.person.delegation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.person.pojo.Person;

@Entity
@Table(name = "DELEGATIONS")
@EntityListeners(AuditingEntityListener.class)
public class Delegations implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "DELEGATION_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_DELEGATION_ID_GENERATOR")
	@SequenceGenerator(name="SEQ_DELEGATION_ID_GENERATOR", sequenceName = "SEQ_DELEGATION_ID_GENERATOR", allocationSize=1)
	private Integer delegationId;

	@Column(name = "DELEGATED_BY")
	private String delegatedBy;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "DELEGATIONS_FK1"), name = "DELEGATED_BY", referencedColumnName = "PERSON_ID", insertable = false, updatable = false)
	private Person delegatedByPerson;

	@Column(name = "DELEGATED_TO")
	private String delegatedTo;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "DELEGATIONS_FK2"), name = "DELEGATED_TO", referencedColumnName = "PERSON_ID", insertable = false, updatable = false)
	private Person delegatedToPerson;

	@Column(name = "EFFECTIVE_DATE")
	private Timestamp effectiveDate;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "DELEGATION_STATUS_CODE")
	private String delegationStatusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "DELEGATIONS_FK3"), name = "DELEGATION_STATUS_CODE", referencedColumnName = "DELEGATION_STATUS_CODE", insertable = false, updatable = false)
	private DelegationStatus delegationStatus;

	@Column(name = "COMMENT")
	private String comment;

	@CreatedDate
	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	private transient String createUserFullName;

	private transient String updateUserFullName;

	public Integer getDelegationId() {
		return delegationId;
	}

	public void setDelegationId(Integer delegationId) {
		this.delegationId = delegationId;
	}

	public String getDelegatedBy() {
		return delegatedBy;
	}

	public void setDelegatedBy(String delegatedBy) {
		this.delegatedBy = delegatedBy;
	}

	public String getDelegatedTo() {
		return delegatedTo;
	}

	public void setDelegatedTo(String delegatedTo) {
		this.delegatedTo = delegatedTo;
	}

	public Timestamp getEffectiveDate() {
		return effectiveDate;
	}

	public void setEffectiveDate(Timestamp effectiveDate) {
		this.effectiveDate = effectiveDate;
	}

	public Timestamp getEndDate() {
		return endDate;
	}

	public void setEndDate(Timestamp endDate) {
		this.endDate = endDate;
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

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Person getDelegatedByPerson() {
		return delegatedByPerson;
	}

	public void setDelegatedByPerson(Person delegatedByPerson) {
		this.delegatedByPerson = delegatedByPerson;
	}

	public Person getDelegatedToPerson() {
		return delegatedToPerson;
	}

	public void setDelegatedToPerson(Person delegatedToPerson) {
		this.delegatedToPerson = delegatedToPerson;
	}

	public String getDelegationStatusCode() {
		return delegationStatusCode;
	}

	public void setDelegationStatusCode(String delegationStatusCode) {
		this.delegationStatusCode = delegationStatusCode;
	}

	public DelegationStatus getDelegationStatus() {
		return delegationStatus;
	}

	public void setDelegationStatus(DelegationStatus delegationStatus) {
		this.delegationStatus = delegationStatus;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public String getCreateUserFullName() {
		return createUserFullName;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setCreateUserFullName(String createUserFullName) {
		this.createUserFullName = createUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

}
