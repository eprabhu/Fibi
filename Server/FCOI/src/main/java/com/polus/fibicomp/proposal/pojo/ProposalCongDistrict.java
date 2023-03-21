package com.polus.fibicomp.proposal.pojo;

import com.fasterxml.jackson.annotation.JsonBackReference;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;

@Entity
@Table(name = "EPS_PROPOSAL_CONG_DISTRICT")
@EntityListeners(AuditingEntityListener.class)
public class ProposalCongDistrict implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @Column(name = "PROPOSAL_CONG_DISTRICT_ID")
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_EPS_PROPOSAL_CONG_DISTRICT")
    @SequenceGenerator(name = "SEQ_EPS_PROPOSAL_CONG_DISTRICT", sequenceName = "SEQ_EPS_PROPOSAL_CONG_DISTRICT", allocationSize=1)
    private Integer proposalCongDistrictId;

    @Column(name = "CONG_DISTRICT_CODE")
    private Integer congDistrictCode;

    @ManyToOne(optional = false)
    @JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_CONG_DISTRICT_FK1"), name = "CONG_DISTRICT_CODE", referencedColumnName = "CONG_DISTRICT_CODE", insertable = false, updatable = false)
    private CongressionalDistrict congressionalDistrict;

    @LastModifiedBy
    @Column(name = "UPDATE_USER")
    private String updateUser;

    @LastModifiedDate
    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimeStamp;

    @JsonBackReference
    @ManyToOne(optional = false)
    @JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_CONG_DISTRICT_FK2"), name = "PROPOSAL_ORGANIZATION_ID", referencedColumnName = "PROPOSAL_ORGANIZATION_ID")
    private ProposalOrganization proposalOrganization;

    public Integer getProposalCongDistrictId() {
        return proposalCongDistrictId;
    }

    public void setProposalCongDistrictId(Integer proposalCongDistrictId) {
        this.proposalCongDistrictId = proposalCongDistrictId;
    }

    public ProposalOrganization getProposalOrganization() {
        return proposalOrganization;
    }

    public void setProposalOrganization(ProposalOrganization proposalOrganization) {
        this.proposalOrganization = proposalOrganization;
    }

    public Integer getCongDistrictCode() {
        return congDistrictCode;
    }

    public void setCongDistrictCode(Integer congDistrictCode) {
        this.congDistrictCode = congDistrictCode;
    }

    public CongressionalDistrict getCongressionalDistrict() {
        return congressionalDistrict;
    }

    public void setCongressionalDistrict(CongressionalDistrict congressionalDistrict) {
        this.congressionalDistrict = congressionalDistrict;
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
}
