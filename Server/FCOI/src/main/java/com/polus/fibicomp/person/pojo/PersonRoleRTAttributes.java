package com.polus.fibicomp.person.pojo;

import java.io.Serializable;
import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Embeddable;

@Embeddable
public class PersonRoleRTAttributes implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@Column(name = "RIGHT_NAME")
	private String rightName;

	public PersonRoleRTAttributes() {
	}

	public PersonRoleRTAttributes(String personId, String unitNumber, String rightName) {
		this.personId = personId;
		this.unitNumber = unitNumber;
		this.rightName = rightName;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public String getRightName() {
		return rightName;
	}

	public void setRightName(String rightName) {
		this.rightName = rightName;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (!(o instanceof PersonRoleRTAttributes))
			return false;
		PersonRoleRTAttributes that = (PersonRoleRTAttributes) o;
		return Objects.equals(getPersonId(), that.getPersonId())
				&& Objects.equals(getUnitNumber(), that.getUnitNumber())
				&& Objects.equals(getRightName(), that.getRightName());
	}

	@Override
	public int hashCode() {
		return Objects.hash(getPersonId(), getUnitNumber(),getRightName());
	}

}
