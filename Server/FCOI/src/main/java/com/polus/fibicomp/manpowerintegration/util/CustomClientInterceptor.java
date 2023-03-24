package com.polus.fibicomp.manpowerintegration.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Map;

import javax.xml.namespace.QName;
import javax.xml.soap.Name;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPEnvelope;
import javax.xml.soap.SOAPHeader;
import javax.xml.soap.SOAPHeaderElement;
import javax.xml.soap.SOAPMessage;
import javax.xml.soap.SOAPPart;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.ws.WebServiceMessage;
import org.springframework.ws.client.WebServiceClientException;
import org.springframework.ws.client.support.interceptor.ClientInterceptor;
import org.springframework.ws.context.MessageContext;
import org.springframework.ws.soap.saaj.SaajSoapMessage;

import com.polus.fibicomp.constants.Constants;

public class CustomClientInterceptor implements ClientInterceptor {

	protected static Logger logger = LogManager.getLogger(CustomClientInterceptor.class.getName());

	private String subscriptionKeyValue;

//	private String ipAddress;

	public CustomClientInterceptor(Map<String, String> workdayConfigDetails) {
		super();
		this.subscriptionKeyValue = workdayConfigDetails.get("SUBSCRIPTION_KEY_VALUE");
//		this.ipAddress = workdayConfigDetails.get("IP_ADDRESS");
	}

	@Override
	public boolean handleRequest(MessageContext messageContext) throws WebServiceClientException {
		try {
			WebServiceMessage message = messageContext.getRequest();
			SaajSoapMessage saajSoapMessage = (SaajSoapMessage) message;
			saajSoapMessage.getSaajMessage().getMimeHeaders().addHeader(Constants.WORKDAY_SUBSCRIPTION_KEY_NAME, subscriptionKeyValue);
//			saajSoapMessage.getSaajMessage().getMimeHeaders().addHeader("X-Forwarded-For", ipAddress);
			SOAPMessage soapMessage = saajSoapMessage.getSaajMessage();
			SOAPPart soapPart = soapMessage.getSOAPPart();
			SOAPEnvelope soapEnvelope = soapPart.getEnvelope();
			SOAPHeader soapHeader = soapEnvelope.getHeader();
			Name headerElementName = soapEnvelope.createName("Security", "wsse", "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd");
			SOAPHeaderElement soapHeaderElement = soapHeader.addHeaderElement(headerElementName);
			soapHeaderElement.setMustUnderstand(true);
			soapHeaderElement.addAttribute(new QName("xmlns:wsu"), "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd");
			SOAPElement usernameTokenSOAPElement = soapHeaderElement.addChildElement("UsernameToken", "wsse");
			usernameTokenSOAPElement.addAttribute(new QName("wsu:Id"), "UsernameToken");
			SOAPElement userNameSOAPElement = usernameTokenSOAPElement.addChildElement("Username", "wsse");
			userNameSOAPElement.addTextNode(Constants.WORKDAY_USERNAME);
			SOAPElement passwordSOAPElement = usernameTokenSOAPElement.addChildElement("Password", "wsse");
			passwordSOAPElement.setAttribute("Type", "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordText");
			passwordSOAPElement.addTextNode(Constants.WORKDAY_PASSWORD);
			QName workdayHeaderElementName = new QName("urn:com.workday/bsvc", "Workday_Common_Header", "bsvc");
			SOAPHeaderElement workdayHeader = soapHeader.addHeaderElement(workdayHeaderElementName);
			SOAPElement workdayHeaderChildSOAPElement = workdayHeader.addChildElement("Include_Reference_Descriptors_In_Response", "bsvc");
			workdayHeaderChildSOAPElement.setValue("true");
			soapMessage.saveChanges();
			ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
			saajSoapMessage.writeTo(byteOut);
			String messageStr = new String(byteOut.toByteArray());
			logger.info(messageStr);
		} catch (Exception e) {
			logger.error("Exception in handleRequest : " + e.getMessage());
		}
		return true;
	}

	@Override
	public boolean handleResponse(MessageContext messageContext) throws WebServiceClientException {
		try {
			SaajSoapMessage saajSoapMessage = (SaajSoapMessage) messageContext.getResponse();
			ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
			saajSoapMessage.writeTo(byteOut);
			String messageStr = new String(byteOut.toByteArray());
			logger.info(messageStr);
		} catch (IOException e) {
			logger.error("Exception in handleResponse : " + e.getMessage());
		}
		return true;
	}

	@Override
	public boolean handleFault(MessageContext messageContext) throws WebServiceClientException {
		try {
			SaajSoapMessage saajSoapMessage = (SaajSoapMessage) messageContext.getResponse();
			ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
			saajSoapMessage.writeTo(byteOut);
			String messageStr = new String(byteOut.toByteArray());
			logger.info(messageStr);
		} catch (IOException e) {
			logger.error("Exception in handleFault : " + e.getMessage());
		}
		return true;
	}

	@Override
	public void afterCompletion(MessageContext messageContext, Exception ex) throws WebServiceClientException {
		// TODO Auto-generated method stub
		
	}

}
