package com.polus.fibicomp.externalreview.pojo;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;

@Entity
@Table(name = "EXT_REVIEW_STATUS")
public class ExtReviewStatus implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @Column(name = "EXT_REVIEW_STATUS_CODE")
    private Integer extReviewStatusCode;

    @Column(name = "DESCRIPTION", nullable = false)
    private String description;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;

    @Column(name = "UPDATE_USER")
    private String updateUser;

    @Column(name = "IS_ACTIVE")
    @Convert(converter = JpaCharBooleanConversion.class)
    private Boolean isActive;

    public Integer getExtReviewStatusCode() {
        return extReviewStatusCode;
    }

    public void setExtReviewStatusCode(Integer extReviewStatusCode) {
        this.extReviewStatusCode = extReviewStatusCode;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
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

    public Boolean getActive() {
        return isActive;
    }

    public void setActive(Boolean active) {
        isActive = active;
    }
}
