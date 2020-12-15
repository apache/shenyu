import intl from 'react-intl-universal'
import locales from './locales'

export function initIntl(lang) {
  intl.init({
    currentLocale: lang,
    locales
  })
}

export function getIntlContent(key) {
  return intl.get(key);
}

export function getCurrentLocale(locale) {
  if (locale === 'en-US') {
    return "English";
  } else {
    return "中文";
  }
}
