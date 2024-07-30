package com.polus.service;

import org.apache.commons.codec.binary.Base64;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.stereotype.Service;

import dev.samstevens.totp.code.CodeGenerator;
import dev.samstevens.totp.code.CodeVerifier;
import dev.samstevens.totp.code.DefaultCodeGenerator;
import dev.samstevens.totp.code.DefaultCodeVerifier;
import dev.samstevens.totp.code.HashingAlgorithm;
import dev.samstevens.totp.exceptions.QrGenerationException;
import dev.samstevens.totp.qr.QrData;
import dev.samstevens.totp.qr.QrGenerator;
import dev.samstevens.totp.qr.ZxingPngQrGenerator;
import dev.samstevens.totp.secret.DefaultSecretGenerator;
import dev.samstevens.totp.time.SystemTimeProvider;
import dev.samstevens.totp.time.TimeProvider;

@Service
public class TwoFactorAuthenticationService {

	protected static Logger log = LogManager.getLogger(TwoFactorAuthenticationService.class.getName());

	private static Base64 base64Codec = new Base64();

	public String generateNewSecret() {
		return new DefaultSecretGenerator().generate();
	}

	public String generateQrCodeImageUri(String secret) {
		QrData data = new QrData.Builder().label("MFA").secret(secret).issuer("Fibi").algorithm(HashingAlgorithm.SHA1)
				.digits(6).period(30).build();

		QrGenerator generator = new ZxingPngQrGenerator();
		byte[] imageData = new byte[0];
		try {
			imageData = generator.generate(data);
		} catch (QrGenerationException e) {
			e.printStackTrace();
			log.error("Error while generating QR-CODE");
		}

		return getDataUriForImage(imageData, generator.getImageMimeType());
	}

	public boolean isOtpValid(String secret, String code) {
		TimeProvider timeProvider = new SystemTimeProvider();
		CodeGenerator codeGenerator = new DefaultCodeGenerator();
		CodeVerifier verifier = new DefaultCodeVerifier(codeGenerator, timeProvider);
		return verifier.isValidCode(secret, code);
	}

	public boolean isOtpNotValid(String secret, String code) {
		return !this.isOtpValid(secret, code);
	}

	public static String getDataUriForImage(byte[] data, String mimeType) {
		String encodedData = new String(base64Codec.encode(data));
		return String.format("data:%s;base64,%s", mimeType, encodedData);
	}
}