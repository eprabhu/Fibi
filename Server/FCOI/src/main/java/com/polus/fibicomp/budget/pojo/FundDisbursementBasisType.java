package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "FUND_DISBURSEMENT_BASIS_TYPE")
public class FundDisbursementBasisType implements  Serializable {

    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "FUND_DISBURSEMENT_BASIS_TYPE_CODE" )
    private String fundDisbursementBasisTypeCode;

    @Column(name = "DESCRIPTION")
    private String description;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;

    @Column(name = "UPDATE_USER")
    private String updateUser;

    public String getFundDisbursementBasisTypeCode() {
        return fundDisbursementBasisTypeCode;
    }

    public void setFundDisbursementBasisTypeCode(String fundDisbursementBasisTypeCode) {
        this.fundDisbursementBasisTypeCode = fundDisbursementBasisTypeCode;
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
}
