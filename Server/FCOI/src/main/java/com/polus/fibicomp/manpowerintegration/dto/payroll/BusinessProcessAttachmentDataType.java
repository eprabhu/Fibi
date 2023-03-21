//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.11 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2020.12.21 at 06:30:11 PM IST 
//


package com.polus.fibicomp.manpowerintegration.dto.payroll;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Element for the attachments pertaining to a Event entered through a web service.
 * 
 * <p>Java class for Business_Process_Attachment_DataType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Business_Process_Attachment_DataType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="File_Name"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *               &lt;maxLength value="255"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="Event_Attachment_Description" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="Event_Attachment_Category_Reference" type="{urn:com.workday/bsvc}Event_Attachment_CategoryObjectType" minOccurs="0"/&gt;
 *         &lt;element name="File" type="{http://www.w3.org/2001/XMLSchema}base64Binary" minOccurs="0"/&gt;
 *         &lt;element name="Content_Type" minOccurs="0"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *               &lt;maxLength value="80"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Business_Process_Attachment_DataType", propOrder = {
    "fileName",
    "eventAttachmentDescription",
    "eventAttachmentCategoryReference",
    "file",
    "contentType"
})
public class BusinessProcessAttachmentDataType {

    @XmlElement(name = "File_Name", required = true)
    protected String fileName;
    @XmlElement(name = "Event_Attachment_Description")
    protected String eventAttachmentDescription;
    @XmlElement(name = "Event_Attachment_Category_Reference")
    protected EventAttachmentCategoryObjectType eventAttachmentCategoryReference;
    @XmlElement(name = "File")
    protected byte[] file;
    @XmlElement(name = "Content_Type")
    protected String contentType;

    /**
     * Gets the value of the fileName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFileName() {
        return fileName;
    }

    /**
     * Sets the value of the fileName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFileName(String value) {
        this.fileName = value;
    }

    /**
     * Gets the value of the eventAttachmentDescription property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getEventAttachmentDescription() {
        return eventAttachmentDescription;
    }

    /**
     * Sets the value of the eventAttachmentDescription property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setEventAttachmentDescription(String value) {
        this.eventAttachmentDescription = value;
    }

    /**
     * Gets the value of the eventAttachmentCategoryReference property.
     * 
     * @return
     *     possible object is
     *     {@link EventAttachmentCategoryObjectType }
     *     
     */
    public EventAttachmentCategoryObjectType getEventAttachmentCategoryReference() {
        return eventAttachmentCategoryReference;
    }

    /**
     * Sets the value of the eventAttachmentCategoryReference property.
     * 
     * @param value
     *     allowed object is
     *     {@link EventAttachmentCategoryObjectType }
     *     
     */
    public void setEventAttachmentCategoryReference(EventAttachmentCategoryObjectType value) {
        this.eventAttachmentCategoryReference = value;
    }

    /**
     * Gets the value of the file property.
     * 
     * @return
     *     possible object is
     *     byte[]
     */
    public byte[] getFile() {
        return file;
    }

    /**
     * Sets the value of the file property.
     * 
     * @param value
     *     allowed object is
     *     byte[]
     */
    public void setFile(byte[] value) {
        this.file = value;
    }

    /**
     * Gets the value of the contentType property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getContentType() {
        return contentType;
    }

    /**
     * Sets the value of the contentType property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setContentType(String value) {
        this.contentType = value;
    }

}
