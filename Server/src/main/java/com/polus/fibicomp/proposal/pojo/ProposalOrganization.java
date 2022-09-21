package com.polus.fibicomp.proposal.pojo;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.pojo.Organization;
import com.polus.fibicomp.pojo.Rolodex;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name = "EPS_PROPOSAL_ORGANIZATION")
@EntityListeners(AuditingEntityListener.class)
public class ProposalOrganization implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @Column(name = "PROPOSAL_ORGANIZATION_ID")
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_EPS_PROPOSAL_ORGANIZATION")
    @SequenceGenerator(name="SEQ_EPS_PROPOSAL_ORGANIZATION", sequenceName = "SEQ_EPS_PROPOSAL_ORGANIZATION", allocationSize=1)
    private Integer proposalOrganizationId;

    @Column(name = "PROPOSAL_ID")
    private Integer proposalId;

    @Column(name = "ORGANIZATION_TYPE_CODE")
    private String organizationTypeCode;

    @Column(name = "ORGANIZATION_ID")
    private String organizationId;

    @ManyToOne(optional = true)
    @JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_SITES_FK1"), name = "ORGANIZATION_ID", referencedColumnName = "ORGANIZATION_ID", insertable = false, updatable = false)
    private Organization organization;

    @ManyToOne(optional = false)
    @JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_SITES_FK2"), name = "ORGANIZATION_TYPE_CODE", referencedColumnName = "ORGANIZATION_TYPE_CODE", insertable = false, updatable = false)
    private OrganizationType organizationType;

    @JsonManagedReference
    @OneToMany(mappedBy = "proposalOrganization", orphanRemoval = true, cascade = { CascadeType.ALL })
    @OrderBy("proposalCongDistrictId ASC")
    private List<ProposalCongDistrict> proposalCongDistricts;

    @Column(name = "ROLODEX_ID")
    private Integer rolodexId;

    @ManyToOne(optional = true, cascade = { CascadeType.REFRESH })
    @JoinColumn(name = "ROLODEX_ID", referencedColumnName = "ROLODEX_ID", insertable = false, updatable = false)
    private Rolodex rolodex;

    @Column(name = "LOCATION")
    private String location;

    @LastModifiedBy
    @Column(name = "UPDATE_USER")
    private String updateUser;

    @LastModifiedDate
    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimeStamp;

    public ProposalOrganization() {
    	proposalCongDistricts= new ArrayList<>();
	}

	public Organization getOrganization() {
        return organization;
    }

    public void setOrganization(Organization organization) {
        this.organization = organization;
    }

    public Integer getProposalOrganizationId() {
        return proposalOrganizationId;
    }

    public void setProposalOrganizationId(Integer proposalOrganizationId) {
        this.proposalOrganizationId = proposalOrganizationId;
    }

    public Integer getProposalId() {
        return proposalId;
    }

    public void setProposalId(Integer proposalId) {
        this.proposalId = proposalId;
    }

    public String getOrganizationTypeCode() {
        return organizationTypeCode;
    }

    public void setOrganizationTypeCode(String organizationTypeCode) {
        this.organizationTypeCode = organizationTypeCode;
    }

    public String getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(String organizationId) {
        this.organizationId = organizationId;
    }

    public OrganizationType getOrganizationType() {
        return organizationType;
    }

    public void setOrganizationType(OrganizationType organizationType) {
        this.organizationType = organizationType;
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

    public List<ProposalCongDistrict> getProposalCongDistricts() {
        return proposalCongDistricts;
    }

    public void setProposalCongDistricts(List<ProposalCongDistrict> proposalCongDistricts) {
        this.proposalCongDistricts = proposalCongDistricts;
    }

    public Integer getRolodexId() {
        return rolodexId;
    }

    public void setRolodexId(Integer rolodexId) {
        this.rolodexId = rolodexId;
    }

    public Rolodex getRolodex() {
        return rolodex;
    }

    public void setRolodex(Rolodex rolodex) {
        this.rolodex = rolodex;
    }

	public String getLocation() {
		return location;
	}

	public void setLocation(String location) {
		this.location = location;
	}
}
