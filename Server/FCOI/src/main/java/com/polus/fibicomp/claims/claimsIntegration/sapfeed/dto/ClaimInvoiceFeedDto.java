package com.polus.fibicomp.claims.claimsIntegration.sapfeed.dto;

import java.sql.Timestamp;

public class ClaimInvoiceFeedDto {

    private Integer feedId;

    private Integer batchId;

    private String claimNumber;

    private String awardNumber;

    private String accountNumber;

    private String piName;

    private String feedType;

    private String feedStatus;

    private String userAction;

    private String updateUser;

    private Timestamp updateTimeStamp;

    private Timestamp createTimeStamp;

    private Timestamp responseTimeStamp;

    private Integer errorCount;

    private Integer countOfRecords;

    private String feedStatusCode;

    private String userActionCode;

    private String feedTypeCode;
    
    private Integer claimId;
    
    private String userActionComment;
    
    private Integer invoiceId;
    
    private Boolean canRevision =  false;
    
    public Integer getFeedId() {
        return feedId;
    }

    public void setFeedId(Integer feedId) {
        this.feedId = feedId;
    }

    public Integer getBatchId() {
        return batchId;
    }

    public void setBatchId(Integer batchId) {
        this.batchId = batchId;
    }

    public String getClaimNumber() {
        return claimNumber;
    }

    public void setClaimNumber(String claimNumber) {
        this.claimNumber = claimNumber;
    }

    public String getAwardNumber() {
        return awardNumber;
    }

    public void setAwardNumber(String awardNumber) {
        this.awardNumber = awardNumber;
    }

    public String getAccountNumber() {
        return accountNumber;
    }

    public void setAccountNumber(String accountNumber) {
        this.accountNumber = accountNumber;
    }

    public String getPiName() {
        return piName;
    }

    public void setPiName(String piName) {
        this.piName = piName;
    }

    public String getFeedType() {
        return feedType;
    }

    public void setFeedType(String feedType) {
        this.feedType = feedType;
    }

    public String getFeedStatus() {
        return feedStatus;
    }

    public void setFeedStatus(String feedStatus) {
        this.feedStatus = feedStatus;
    }

    public String getUserAction() {
        return userAction;
    }

    public void setUserAction(String userAction) {
        this.userAction = userAction;
    }

    public String getUpdateUser() {
        return updateUser;
    }

    public void setUpdateUser(String updateUser) {
        this.updateUser = updateUser;
    }

    public Timestamp getUpdateTimeStamp() {
        return updateTimeStamp;
    }

    public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
        this.updateTimeStamp = updateTimeStamp;
    }

	public Timestamp getCreateTimeStamp() {
		return createTimeStamp;
	}

	public void setCreateTimeStamp(Timestamp createTimeStamp) {
		this.createTimeStamp = createTimeStamp;
	}

	public Timestamp getResponseTimeStamp() {
		return responseTimeStamp;
	}

	public void setResponseTimeStamp(Timestamp responseTimeStamp) {
		this.responseTimeStamp = responseTimeStamp;
	}

	public Integer getErrorCount() {
		return errorCount;
	}

	public void setErrorCount(Integer errorCount) {
		this.errorCount = errorCount;
	}

	public Integer getCountOfRecords() {
		return countOfRecords;
	}

	public void setCountOfRecords(Integer countOfRecords) {
		this.countOfRecords = countOfRecords;
	}

	public String getFeedStatusCode() {
		return feedStatusCode;
	}

	public void setFeedStatusCode(String feedStatusCode) {
		this.feedStatusCode = feedStatusCode;
	}

	public String getUserActionCode() {
		return userActionCode;
	}

	public void setUserActionCode(String userActionCode) {
		this.userActionCode = userActionCode;
	}

	public String getFeedTypeCode() {
		return feedTypeCode;
	}

	public void setFeedTypeCode(String feedTypeCode) {
		this.feedTypeCode = feedTypeCode;
	}

	public Integer getClaimId() {
		return claimId;
	}

	public void setClaimId(Integer claimId) {
		this.claimId = claimId;
	}

	public String getUserActionComment() {
		return userActionComment;
	}

	public void setUserActionComment(String userActionComment) {
		this.userActionComment = userActionComment;
	}

	public Integer getInvoiceId() {
		return invoiceId;
	}

	public void setInvoiceId(Integer invoiceId) {
		this.invoiceId = invoiceId;
	}

	public Boolean getCanRevision() {
		return canRevision;
	}

	public void setCanRevision(Boolean canRevision) {
		this.canRevision = canRevision;
	}
}
