package config;

import org.springframework.security.oauth2.client.OAuth2AuthorizedClient;
import org.springframework.security.oauth2.client.authentication.OAuth2AuthenticationToken;
import org.springframework.security.oauth2.client.registration.ClientRegistration;
import org.springframework.security.oauth2.client.registration.ClientRegistrations;
import org.springframework.security.oauth2.core.AuthorizationGrantType;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.security.oauth2.core.user.DefaultOAuth2User;
import org.springframework.security.oauth2.core.user.OAuth2UserAuthority;

import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.springframework.security.oauth2.core.OAuth2AccessToken.TokenType.BEARER;

public class OAuth2PluginTestConfig {

    private static final String REGISTRATION_ID = "shenyu";

    public static OAuth2AuthenticationToken newToken() {
        Map<String, Object> attributes = Collections.singletonMap("read", "user");
        List<OAuth2UserAuthority> oAuth2UserAuthorities = Collections.singletonList(new OAuth2UserAuthority(attributes));
        DefaultOAuth2User defaultOAuth2User = new DefaultOAuth2User(oAuth2UserAuthorities, attributes, "read");
        return new OAuth2AuthenticationToken(defaultOAuth2User, oAuth2UserAuthorities, REGISTRATION_ID);
    }

    public static OAuth2AuthorizedClient newClient() {
        OAuth2AccessToken oAuth2AccessToken = new OAuth2AccessToken(BEARER, "TestToken", Instant.now(), null);
        return new OAuth2AuthorizedClient(newRegistration(), "testUser", oAuth2AccessToken);
    }

    private static ClientRegistration newRegistration() {
        ClientRegistration.Builder shenyuClientRegistration = ClientRegistration.withRegistrationId(REGISTRATION_ID);
        shenyuClientRegistration.authorizationGrantType(AuthorizationGrantType.AUTHORIZATION_CODE);
        shenyuClientRegistration.tokenUri("/");
        shenyuClientRegistration.authorizationUri("/");
        shenyuClientRegistration.redirectUriTemplate("/");
        shenyuClientRegistration.scope("read:user");
        shenyuClientRegistration.userInfoUri("/");
        shenyuClientRegistration.clientId("shenyu");
        shenyuClientRegistration.clientSecret("shenyu");
        shenyuClientRegistration.redirectUriTemplate("{baseUrl}/login/oauth2/code/{registrationId}");
        return shenyuClientRegistration.build();
    }
}
