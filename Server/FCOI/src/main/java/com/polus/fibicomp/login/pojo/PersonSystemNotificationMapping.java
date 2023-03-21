package com.polus.fibicomp.login.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.IdClass;
import javax.persistence.Table;

import org.apache.commons.lang3.builder.CompareToBuilder;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

@Entity
@Table(name = "PERSON_SYSTEM_NOTIFICATION_MAPPING")
@IdClass(PersonSystemNotificationMapping.personId.class)
public class PersonSystemNotificationMapping implements Serializable, Comparable<PersonSystemNotificationMapping> {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PERSON_ID")
	private String personId;

	@Id
	@Column(name = "SYSTEM_NOTIFICATION_ID")
	private Integer systemNotificationId;
	
	@Column(name = "READ_TIMESTAMP")
	private Timestamp readTimestamp;

	public String getPersonId() {
		return personId;
	}


	public void setPersonId(String personId) {
		this.personId = personId;
	}


	public Integer getSystemNotificationId() {
		return systemNotificationId;
	}


	public void setSystemNotificationId(Integer systemNotificationId) {
		this.systemNotificationId = systemNotificationId;
	}


	public Timestamp getReadTimestamp() {
		return readTimestamp;
	}


	public void setReadTimestamp(Timestamp readTimestamp) {
		this.readTimestamp = readTimestamp;
	}


	@Override
	public int compareTo(PersonSystemNotificationMapping personSystemNotificationMapping) {
		return new CompareToBuilder().append(this.personId, personSystemNotificationMapping.personId)
				.append(this.systemNotificationId, personSystemNotificationMapping.systemNotificationId).toComparison();
	}

	public static final class personId implements Serializable, Comparable<personId> {

		private static final long serialVersionUID = 1L;
		private String personId;
		private Integer systemNotificationId;

		public String getPersonId() {
			return personId;
		}
		public void setPersonId(String personId) {
			this.personId = personId;
		}
		public Integer getSystemNotificationId() {
			return systemNotificationId;
		}
		public void setSystemNotificationId(Integer systemNotificationId) {
			this.systemNotificationId = systemNotificationId;
		}
	
		@Override
		public String toString() {
			return new ToStringBuilder(this).append("personId", this.personId)
					.append("systemNotificationId", this.systemNotificationId).toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final personId pId = (personId) other;
			return new EqualsBuilder().append(this.personId, pId.personId)
					.append(this.systemNotificationId, pId.systemNotificationId).isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.personId).append(this.personId)
					.append(this.systemNotificationId).append(this.systemNotificationId).toHashCode();
		}

		@Override
		public int compareTo(personId other) {
			return new CompareToBuilder().append(this.personId, other.personId)
					.append(this.systemNotificationId, other.systemNotificationId).toComparison();
		}

	}

}
