package com.polus.fibicomp.proposal.pojo;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;

@Entity
@Table(name = "CONGRESSIONAL_DISTRICT")
@EntityListeners(AuditingEntityListener.class)
public class CongressionalDistrict implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @Column(name = "CONG_DISTRICT_CODE")
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_CONGRESSIONAL_DISTRICT")
    @SequenceGenerator(name = "SEQ_CONGRESSIONAL_DISTRICT", sequenceName = "SEQ_CONGRESSIONAL_DISTRICT", allocationSize=1)
    private Integer congDistrictCode;

    @Column(name = "DESCRIPTION")
    private String description;

    @Column(name = "IS_ACTIVE")
    private String isActive;

    @LastModifiedDate
    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimeStamp;

    @LastModifiedBy
    @Column(name = "UPDATE_USER")
    private String updateUser;

    public String getIsActive() {
        return isActive;
    }

    public void setIsActive(String isActive) {
        this.isActive = isActive;
    }

    public Integer getCongDistrictCode() {
        return congDistrictCode;
    }

    public void setCongDistrictCode(Integer congDistrictCode) {
        this.congDistrictCode = congDistrictCode;
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
}
