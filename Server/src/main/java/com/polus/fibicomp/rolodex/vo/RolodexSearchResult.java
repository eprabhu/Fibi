package com.polus.fibicomp.rolodex.vo;

public class RolodexSearchResult {

	private Integer rolodexId;

	private String firstName;

	private String middleName;

	private String lastName;

	private String prefix;

	public RolodexSearchResult(Integer rolodexId, String firstName, String middleName, String lastName, String prefix) {
		super();
		this.rolodexId = rolodexId;
		this.firstName = firstName;
		this.middleName = middleName;
		this.lastName = lastName;
		this.prefix = prefix;
		this.getFullName();
	}

	public Integer getRolodexId() {
		return rolodexId;
	}

	public void setRolodexId(Integer rolodexId) {
		this.rolodexId = rolodexId;
	}

	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public String getMiddleName() {
		return middleName;
	}

	public void setMiddleName(String middleName) {
		this.middleName = middleName;
	}

	public String getLastName() {
		return lastName;
	}

	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	public String getFullName() {
		final StringBuilder name = new StringBuilder();
		if (this.getLastName() != null) {
			name.append(this.getLastName());
			name.append(", ");
		}
		if (this.getPrefix() != null) {
			name.append(this.getPrefix());
			name.append(" ");
		}
		if (this.getFirstName() != null) {
			name.append(this.getFirstName());
			name.append(" ");
		}
		if (this.getMiddleName() != null) {
			name.append(this.getMiddleName());
		}
		return name.length() > 0 ? name.toString() : null;
	}

	public String getPrefix() {
		return prefix;
	}

	public void setPrefix(String prefix) {
		this.prefix = prefix;
	}

}
