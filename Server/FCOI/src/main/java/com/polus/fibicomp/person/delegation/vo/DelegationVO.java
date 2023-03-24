package com.polus.fibicomp.person.delegation.vo;

import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.person.delegation.pojo.Delegations;
import com.polus.fibicomp.person.pojo.Person;

public class DelegationVO {

	private String personId;

	private Delegations delegation;

	private List<Delegations> delegations;

	private Integer delegationId;

	private String delegationStatusCode;

	private Boolean isMaintainDelegationRightExist = Boolean.FALSE;

	private Person person;

	private String previousDelegatedToPersonId;

	public DelegationVO() {
		delegations = new ArrayList<>();
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Delegations getDelegation() {
		return delegation;
	}

	public void setDelegation(Delegations delegation) {
		this.delegation = delegation;
	}

	public List<Delegations> getDelegations() {
		return delegations;
	}

	public void setDelegations(List<Delegations> delegations) {
		this.delegations = delegations;
	}

	public Integer getDelegationId() {
		return delegationId;
	}

	public void setDelegationId(Integer delegationId) {
		this.delegationId = delegationId;
	}

	public String getDelegationStatusCode() {
		return delegationStatusCode;
	}

	public void setDelegationStatusCode(String delegationStatusCode) {
		this.delegationStatusCode = delegationStatusCode;
	}

	public Boolean getIsMaintainDelegationRightExist() {
		return isMaintainDelegationRightExist;
	}

	public void setIsMaintainDelegationRightExist(Boolean isMaintainDelegationRightExist) {
		this.isMaintainDelegationRightExist = isMaintainDelegationRightExist;
	}

	public Person getPerson() {
		return person;
	}

	public void setPerson(Person person) {
		this.person = person;
	}

	public String getPreviousDelegatedToPersonId() {
		return previousDelegatedToPersonId;
	}

	public void setPreviousDelegatedToPersonId(String previousDelegatedToPersonId) {
		this.previousDelegatedToPersonId = previousDelegatedToPersonId;
	}

}
