package com.polus.fibicomp.person.vo;

public class PersonSearchResult {
	
	private String personId;
	
	public PersonSearchResult(String personId, String fullName) {
		super();
		this.personId = personId;
		this.fullName = fullName;
	}

	private String fullName;

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

}
