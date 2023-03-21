package com.polus.fibicomp.externalreview.pojo;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;

@Entity
@Table(name = "EXT_REVIEW_SERVICE_TYPE")
public class ExtReviewServiceType implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @Column(name = "EXT_REVIEW_SERVICE_TYPE_CODE")
    private String extReviewServiceTypeCode;

    @Column(name = "DESCRIPTION", nullable = false)
    private String description;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;

    @Column(name = "UPDATE_USER")
    private String updateUser;

    @Column(name = "IS_ACTIVE")
    @Convert(converter = JpaCharBooleanConversion.class)
    private Boolean isActive;

    @Column(name = "IS_SCORING_NEEDED")
    @Convert(converter = JpaCharBooleanConversion.class)
    private Boolean isScoringNeeded;

    public String getExtReviewServiceTypeCode() {
        return extReviewServiceTypeCode;
    }

    public void setExtReviewServiceTypeCode(String extReviewServiceTypeCode) {
        this.extReviewServiceTypeCode = extReviewServiceTypeCode;
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

	public Boolean getIsScoringNeeded() {
		return isScoringNeeded;
	}

	public void setIsScoringNeeded(Boolean isScoringNeeded) {
		this.isScoringNeeded = isScoringNeeded;
	}

}
