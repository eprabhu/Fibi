package com.polus.fibicomp.adminportal.pojo;


import com.polus.fibicomp.pojo.Sponsor;
import org.hibernate.annotations.GenericGenerator;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

/**
 * SponsorHierarchy Entity class
 *
 * @createdBy Ajin
 * @date 23 Nov 2021
 */
@Entity
@Table(name = "SPONSOR_HIERARCHY")
public class SponsorHierarchy implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @GenericGenerator(name = "SponsorHierarchyIdGenerator", strategy = "increment", parameters = {
            @org.hibernate.annotations.Parameter(name = "initial_value", value = "1"), @org.hibernate.annotations.Parameter(name = "increment_size", value = "1")})
    @GeneratedValue(generator = "SponsorHierarchyIdGenerator")
    @Column(name = "SPONSOR_GROUP_ID")
    private Integer sponsorGroupId;

    @Column(name = "SPONSOR_GROUP_NAME")
    private String sponsorGroupName;

    @Column(name = "SPONSOR_ORIGINATING_GROUP_ID")
    private Integer sponsorOriginatingGroupId;

    @ManyToOne(cascade = {CascadeType.REFRESH}, fetch = FetchType.LAZY)
    @JoinColumn(foreignKey = @ForeignKey(name = "SPON_HIER_GROUP_FK2"), name = "SPONSOR_ORIGINATING_GROUP_ID", insertable = false, updatable = false)
    private SponsorHierarchy originatingSponsorHierarchy;

    @Column(name = "SPONSOR_ROOT_GROUP_ID")
    private Integer sponsorRootGroupId;

    @Column(name = "SPONSOR_CODE")
    private String sponsorCode;

    @ManyToOne(cascade = {CascadeType.REFRESH})
    @JoinColumn(foreignKey = @ForeignKey(name = "SPON_HIER_SPON_FK1"), name = "SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
    private Sponsor sponsor;

    @OrderBy("orderNumber DESC")
    @OneToMany(mappedBy = "originatingSponsorHierarchy", cascade = CascadeType.ALL, fetch = FetchType.LAZY, orphanRemoval = true)
    private List<SponsorHierarchy> childSponsorHierarchies;

    @Column(name = "ORDER_NUMBER")
    private Integer orderNumber;

    @CreatedDate
    @Column(name = "CREATE_TIMESTAMP")
    private Timestamp createTimestamp;

    @CreatedBy
    @Column(name = "CREATE_USER")
    private String createUser;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;

    @Column(name = "UPDATE_USER")
    private String updateUser;

    public Integer getSponsorGroupId() {
        return sponsorGroupId;
    }

    public void setSponsorGroupId(Integer sponsorGroupId) {
        this.sponsorGroupId = sponsorGroupId;
    }

    public String getSponsorGroupName() {
        return sponsorGroupName;
    }

    public void setSponsorGroupName(String sponsorGroupName) {
        this.sponsorGroupName = sponsorGroupName;
    }

    public Integer getSponsorOriginatingGroupId() {
        return sponsorOriginatingGroupId;
    }

    public void setSponsorOriginatingGroupId(Integer sponsorOriginatingGroupId) {
        this.sponsorOriginatingGroupId = sponsorOriginatingGroupId;
    }

    public SponsorHierarchy getOriginatingSponsorHierarchy() {
        return originatingSponsorHierarchy;
    }

    public void setOriginatingSponsorHierarchy(SponsorHierarchy originatingSponsorHierarchy) {
        this.originatingSponsorHierarchy = originatingSponsorHierarchy;
    }

    public Integer getSponsorRootGroupId() {
        return sponsorRootGroupId;
    }

    public void setSponsorRootGroupId(Integer sponsorRootGroupId) {
        this.sponsorRootGroupId = sponsorRootGroupId;
    }

    public String getSponsorCode() {
        return sponsorCode;
    }

    public void setSponsorCode(String sponsorCode) {
        this.sponsorCode = sponsorCode;
    }

    public Sponsor getSponsor() {
        return sponsor;
    }

    public void setSponsor(Sponsor sponsor) {
        this.sponsor = sponsor;
    }

    public List<SponsorHierarchy> getChildSponsorHierarchies() {
        return childSponsorHierarchies;
    }

    public void setChildSponsorHierarchies(List<SponsorHierarchy> childSponsorHierarchies) {
        this.childSponsorHierarchies = childSponsorHierarchies;
    }

    public Integer getOrderNumber() {
        return orderNumber;
    }

    public void setOrderNumber(Integer orderNumber) {
        this.orderNumber = orderNumber;
    }

    public Timestamp getCreateTimestamp() {
        return createTimestamp;
    }

    public void setCreateTimestamp(Timestamp createTimestamp) {
        this.createTimestamp = createTimestamp;
    }

    public String getCreateUser() {
        return createUser;
    }

    public void setCreateUser(String createUser) {
        this.createUser = createUser;
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
