//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.11 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2020.12.21 at 06:30:11 PM IST 
//


package com.polus.fibicomp.manpowerintegration.dto.payroll;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Details of the allocation, (e.g., a set of allocation dimensions and percentages)
 * 
 * <p>Java class for Costing_Allocation_Detail_Replacement_DataType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Costing_Allocation_Detail_Replacement_DataType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="Order" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="Default_from_Organization_Assignment" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/&gt;
 *         &lt;element name="Costing_Override_Company_Reference" type="{urn:com.workday/bsvc}CompanyObjectType" minOccurs="0"/&gt;
 *         &lt;element name="Costing_Override_Worktag_Reference" type="{urn:com.workday/bsvc}Tenanted_Payroll_WorktagObjectType" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="Distribution_Percent"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal"&gt;
 *               &lt;totalDigits value="9"/&gt;
 *               &lt;minInclusive value="0"/&gt;
 *               &lt;fractionDigits value="6"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="Salary_Over_the_Cap_Costing_Allocation_Detail_Data" type="{urn:com.workday/bsvc}Salary_Over_the_Cap_Costing_Allocation_Detail_DataType" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Costing_Allocation_Detail_Replacement_DataType", propOrder = {
    "order",
    "defaultFromOrganizationAssignment",
    "costingOverrideCompanyReference",
    "costingOverrideWorktagReference",
    "distributionPercent",
    "salaryOverTheCapCostingAllocationDetailData"
})
public class CostingAllocationDetailReplacementDataType {

    @XmlElement(name = "Order", required = true)
    protected String order;
    @XmlElement(name = "Default_from_Organization_Assignment")
    protected Boolean defaultFromOrganizationAssignment;
    @XmlElement(name = "Costing_Override_Company_Reference")
    protected CompanyObjectType costingOverrideCompanyReference;
    @XmlElement(name = "Costing_Override_Worktag_Reference")
    protected List<TenantedPayrollWorktagObjectType> costingOverrideWorktagReference;
    @XmlElement(name = "Distribution_Percent", required = true)
    protected BigDecimal distributionPercent;
    @XmlElement(name = "Salary_Over_the_Cap_Costing_Allocation_Detail_Data")
    protected List<SalaryOverTheCapCostingAllocationDetailDataType> salaryOverTheCapCostingAllocationDetailData;

    /**
     * Gets the value of the order property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getOrder() {
        return order;
    }

    /**
     * Sets the value of the order property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setOrder(String value) {
        this.order = value;
    }

    /**
     * Gets the value of the defaultFromOrganizationAssignment property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isDefaultFromOrganizationAssignment() {
        return defaultFromOrganizationAssignment;
    }

    /**
     * Sets the value of the defaultFromOrganizationAssignment property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setDefaultFromOrganizationAssignment(Boolean value) {
        this.defaultFromOrganizationAssignment = value;
    }

    /**
     * Gets the value of the costingOverrideCompanyReference property.
     * 
     * @return
     *     possible object is
     *     {@link CompanyObjectType }
     *     
     */
    public CompanyObjectType getCostingOverrideCompanyReference() {
        return costingOverrideCompanyReference;
    }

    /**
     * Sets the value of the costingOverrideCompanyReference property.
     * 
     * @param value
     *     allowed object is
     *     {@link CompanyObjectType }
     *     
     */
    public void setCostingOverrideCompanyReference(CompanyObjectType value) {
        this.costingOverrideCompanyReference = value;
    }

    /**
     * Gets the value of the costingOverrideWorktagReference property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the costingOverrideWorktagReference property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getCostingOverrideWorktagReference().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link TenantedPayrollWorktagObjectType }
     * 
     * 
     */
    public List<TenantedPayrollWorktagObjectType> getCostingOverrideWorktagReference() {
        if (costingOverrideWorktagReference == null) {
            costingOverrideWorktagReference = new ArrayList<TenantedPayrollWorktagObjectType>();
        }
        return this.costingOverrideWorktagReference;
    }

    /**
     * Gets the value of the distributionPercent property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getDistributionPercent() {
        return distributionPercent;
    }

    /**
     * Sets the value of the distributionPercent property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setDistributionPercent(BigDecimal value) {
        this.distributionPercent = value;
    }

    /**
     * Gets the value of the salaryOverTheCapCostingAllocationDetailData property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the salaryOverTheCapCostingAllocationDetailData property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getSalaryOverTheCapCostingAllocationDetailData().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link SalaryOverTheCapCostingAllocationDetailDataType }
     * 
     * 
     */
    public List<SalaryOverTheCapCostingAllocationDetailDataType> getSalaryOverTheCapCostingAllocationDetailData() {
        if (salaryOverTheCapCostingAllocationDetailData == null) {
            salaryOverTheCapCostingAllocationDetailData = new ArrayList<SalaryOverTheCapCostingAllocationDetailDataType>();
        }
        return this.salaryOverTheCapCostingAllocationDetailData;
    }

}
