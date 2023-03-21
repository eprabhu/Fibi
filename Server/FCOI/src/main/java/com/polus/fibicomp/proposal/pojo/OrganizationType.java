package com.polus.fibicomp.proposal.pojo;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;

@Entity
@Table(name = "ORGANIZATION_TYPE")
public class OrganizationType implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @Column(name = "ORGANIZATION_TYPE_CODE")
    private String organizationTypeCode;

    @Column(name = "DESCRIPTION")
    private String description;

    @Column(name = "IS_ACTIVE")
    @Convert(converter = JpaCharBooleanConversion.class)
    private Boolean isActive;

    @Column(name = "IS_DEFAULT_ENTRY_REQUIRED")
    @Convert(converter = JpaCharBooleanConversion.class)
    private Boolean isDefaultEntryRequired;

    @JsonIgnore
    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimeStamp;

    @JsonIgnore
    @Column(name = "UPDATE_USER")
    private String updateUser;

    public String getOrganizationTypeCode() {
        return organizationTypeCode;
    }

    public void setOrganizationTypeCode(String organizationTypeCode) {
        this.organizationTypeCode = organizationTypeCode;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
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

	public Boolean getIsDefaultEntryRequired() {
		return isDefaultEntryRequired;
	}

	public void setIsDefaultEntryRequired(Boolean isDefaultEntryRequired) {
		this.isDefaultEntryRequired = isDefaultEntryRequired;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}
}
