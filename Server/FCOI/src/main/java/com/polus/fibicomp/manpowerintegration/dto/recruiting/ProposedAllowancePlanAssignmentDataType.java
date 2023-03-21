//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.11 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2020.11.25 at 01:02:19 PM IST 
//


package com.polus.fibicomp.manpowerintegration.dto.recruiting;

import java.math.BigDecimal;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.datatype.XMLGregorianCalendar;


/**
 * Encapsulating element containing all Allowance Plan Compensation data.
 * 
 * <p>Java class for Proposed_Allowance_Plan_Assignment_DataType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Proposed_Allowance_Plan_Assignment_DataType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="Allowance_Plan_Reference" type="{urn:com.workday/bsvc}Allowance_Value_PlanObjectType" minOccurs="0"/&gt;
 *         &lt;choice&gt;
 *           &lt;element name="Percent" minOccurs="0"&gt;
 *             &lt;simpleType&gt;
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal"&gt;
 *                 &lt;totalDigits value="20"/&gt;
 *                 &lt;minInclusive value="0"/&gt;
 *                 &lt;fractionDigits value="10"/&gt;
 *               &lt;/restriction&gt;
 *             &lt;/simpleType&gt;
 *           &lt;/element&gt;
 *           &lt;element name="Amount" minOccurs="0"&gt;
 *             &lt;simpleType&gt;
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal"&gt;
 *                 &lt;totalDigits value="26"/&gt;
 *                 &lt;fractionDigits value="6"/&gt;
 *               &lt;/restriction&gt;
 *             &lt;/simpleType&gt;
 *           &lt;/element&gt;
 *         &lt;/choice&gt;
 *         &lt;element name="Manage_by_Compensation_Basis_Override_Amount" minOccurs="0"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal"&gt;
 *               &lt;totalDigits value="26"/&gt;
 *               &lt;fractionDigits value="6"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="Currency_Reference" type="{urn:com.workday/bsvc}CurrencyObjectType" minOccurs="0"/&gt;
 *         &lt;element name="Frequency_Reference" type="{urn:com.workday/bsvc}FrequencyObjectType" minOccurs="0"/&gt;
 *         &lt;element name="Expected_End_Date" type="{http://www.w3.org/2001/XMLSchema}date" minOccurs="0"/&gt;
 *         &lt;element name="Reimbursement_Start_Date" type="{http://www.w3.org/2001/XMLSchema}date" minOccurs="0"/&gt;
 *         &lt;element name="Actual_End_Date" type="{http://www.w3.org/2001/XMLSchema}date" minOccurs="0"/&gt;
 *         &lt;element name="Fixed_for_Manage_by_Basis_Total" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Proposed_Allowance_Plan_Assignment_DataType", propOrder = {
    "allowancePlanReference",
    "percent",
    "amount",
    "manageByCompensationBasisOverrideAmount",
    "currencyReference",
    "frequencyReference",
    "expectedEndDate",
    "reimbursementStartDate",
    "actualEndDate",
    "fixedForManageByBasisTotal"
})
public class ProposedAllowancePlanAssignmentDataType {

    @XmlElement(name = "Allowance_Plan_Reference")
    protected AllowanceValuePlanObjectType allowancePlanReference;
    @XmlElement(name = "Percent")
    protected BigDecimal percent;
    @XmlElement(name = "Amount")
    protected BigDecimal amount;
    @XmlElement(name = "Manage_by_Compensation_Basis_Override_Amount")
    protected BigDecimal manageByCompensationBasisOverrideAmount;
    @XmlElement(name = "Currency_Reference")
    protected CurrencyObjectType currencyReference;
    @XmlElement(name = "Frequency_Reference")
    protected FrequencyObjectType frequencyReference;
    @XmlElement(name = "Expected_End_Date")
    @XmlSchemaType(name = "date")
    protected XMLGregorianCalendar expectedEndDate;
    @XmlElement(name = "Reimbursement_Start_Date")
    @XmlSchemaType(name = "date")
    protected XMLGregorianCalendar reimbursementStartDate;
    @XmlElement(name = "Actual_End_Date")
    @XmlSchemaType(name = "date")
    protected XMLGregorianCalendar actualEndDate;
    @XmlElement(name = "Fixed_for_Manage_by_Basis_Total")
    protected Boolean fixedForManageByBasisTotal;

    /**
     * Gets the value of the allowancePlanReference property.
     * 
     * @return
     *     possible object is
     *     {@link AllowanceValuePlanObjectType }
     *     
     */
    public AllowanceValuePlanObjectType getAllowancePlanReference() {
        return allowancePlanReference;
    }

    /**
     * Sets the value of the allowancePlanReference property.
     * 
     * @param value
     *     allowed object is
     *     {@link AllowanceValuePlanObjectType }
     *     
     */
    public void setAllowancePlanReference(AllowanceValuePlanObjectType value) {
        this.allowancePlanReference = value;
    }

    /**
     * Gets the value of the percent property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getPercent() {
        return percent;
    }

    /**
     * Sets the value of the percent property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setPercent(BigDecimal value) {
        this.percent = value;
    }

    /**
     * Gets the value of the amount property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getAmount() {
        return amount;
    }

    /**
     * Sets the value of the amount property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setAmount(BigDecimal value) {
        this.amount = value;
    }

    /**
     * Gets the value of the manageByCompensationBasisOverrideAmount property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getManageByCompensationBasisOverrideAmount() {
        return manageByCompensationBasisOverrideAmount;
    }

    /**
     * Sets the value of the manageByCompensationBasisOverrideAmount property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setManageByCompensationBasisOverrideAmount(BigDecimal value) {
        this.manageByCompensationBasisOverrideAmount = value;
    }

    /**
     * Gets the value of the currencyReference property.
     * 
     * @return
     *     possible object is
     *     {@link CurrencyObjectType }
     *     
     */
    public CurrencyObjectType getCurrencyReference() {
        return currencyReference;
    }

    /**
     * Sets the value of the currencyReference property.
     * 
     * @param value
     *     allowed object is
     *     {@link CurrencyObjectType }
     *     
     */
    public void setCurrencyReference(CurrencyObjectType value) {
        this.currencyReference = value;
    }

    /**
     * Gets the value of the frequencyReference property.
     * 
     * @return
     *     possible object is
     *     {@link FrequencyObjectType }
     *     
     */
    public FrequencyObjectType getFrequencyReference() {
        return frequencyReference;
    }

    /**
     * Sets the value of the frequencyReference property.
     * 
     * @param value
     *     allowed object is
     *     {@link FrequencyObjectType }
     *     
     */
    public void setFrequencyReference(FrequencyObjectType value) {
        this.frequencyReference = value;
    }

    /**
     * Gets the value of the expectedEndDate property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getExpectedEndDate() {
        return expectedEndDate;
    }

    /**
     * Sets the value of the expectedEndDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setExpectedEndDate(XMLGregorianCalendar value) {
        this.expectedEndDate = value;
    }

    /**
     * Gets the value of the reimbursementStartDate property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getReimbursementStartDate() {
        return reimbursementStartDate;
    }

    /**
     * Sets the value of the reimbursementStartDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setReimbursementStartDate(XMLGregorianCalendar value) {
        this.reimbursementStartDate = value;
    }

    /**
     * Gets the value of the actualEndDate property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getActualEndDate() {
        return actualEndDate;
    }

    /**
     * Sets the value of the actualEndDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setActualEndDate(XMLGregorianCalendar value) {
        this.actualEndDate = value;
    }

    /**
     * Gets the value of the fixedForManageByBasisTotal property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isFixedForManageByBasisTotal() {
        return fixedForManageByBasisTotal;
    }

    /**
     * Sets the value of the fixedForManageByBasisTotal property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setFixedForManageByBasisTotal(Boolean value) {
        this.fixedForManageByBasisTotal = value;
    }

}
