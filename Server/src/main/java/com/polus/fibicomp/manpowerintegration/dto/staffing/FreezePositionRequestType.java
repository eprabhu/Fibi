//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.11 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2020.11.25 at 05:00:45 PM IST 
//


package com.polus.fibicomp.manpowerintegration.dto.staffing;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Wrapper element for the Freeze Position business process.
 * 
 * <p>Java class for Freeze_Position_RequestType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Freeze_Position_RequestType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="Freeze_Position_Data" type="{urn:com.workday/bsvc}Position_Group_Freeze_DataType"/&gt;
 *         &lt;element name="Business_Process_Parameters" type="{urn:com.workday/bsvc}Business_Process_ParametersType" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute ref="{urn:com.workday/bsvc}version"/&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Freeze_Position_RequestType", propOrder = {
    "freezePositionData",
    "businessProcessParameters"
})
public class FreezePositionRequestType {

    @XmlElement(name = "Freeze_Position_Data", required = true)
    protected PositionGroupFreezeDataType freezePositionData;
    @XmlElement(name = "Business_Process_Parameters")
    protected BusinessProcessParametersType businessProcessParameters;
    @XmlAttribute(name = "version", namespace = "urn:com.workday/bsvc")
    protected String version;

    /**
     * Gets the value of the freezePositionData property.
     * 
     * @return
     *     possible object is
     *     {@link PositionGroupFreezeDataType }
     *     
     */
    public PositionGroupFreezeDataType getFreezePositionData() {
        return freezePositionData;
    }

    /**
     * Sets the value of the freezePositionData property.
     * 
     * @param value
     *     allowed object is
     *     {@link PositionGroupFreezeDataType }
     *     
     */
    public void setFreezePositionData(PositionGroupFreezeDataType value) {
        this.freezePositionData = value;
    }

    /**
     * Gets the value of the businessProcessParameters property.
     * 
     * @return
     *     possible object is
     *     {@link BusinessProcessParametersType }
     *     
     */
    public BusinessProcessParametersType getBusinessProcessParameters() {
        return businessProcessParameters;
    }

    /**
     * Sets the value of the businessProcessParameters property.
     * 
     * @param value
     *     allowed object is
     *     {@link BusinessProcessParametersType }
     *     
     */
    public void setBusinessProcessParameters(BusinessProcessParametersType value) {
        this.businessProcessParameters = value;
    }

    /**
     * Gets the value of the version property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getVersion() {
        return version;
    }

    /**
     * Sets the value of the version property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setVersion(String value) {
        this.version = value;
    }

}
