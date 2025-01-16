/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const {fbContent} = require('docusaurus-plugin-internaldocs-fb/internal');

/* List of projects/orgs using Pysa for the users page */

const users = [
  {
    caption: 'Instagram',
    image: '/pyre/img/ig.png',
    infoLink: 'https://www.instagram.com',
    pinned: true,
  },
];

/** @type {import('@docusaurus/types').DocusaurusConfig} */
(module.exports = {
  title: 'Pysa',
  tagline: 'Python Static Analyzer',
  url: 'https://pysa-security.org',
  baseUrl: '/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  trailingSlash: true,
  organizationName: 'facebook',
  projectName: 'pysa',
  favicon: 'img/favicon.png',
  scripts: ['https://buttons.github.io/buttons.js'],

  presets: [
    [
      require.resolve('docusaurus-plugin-internaldocs-fb/docusaurus-preset'),
      {
        docs: {
          // Docs folder path relative to website dir.
          path: 'docs',
          // Sidebars file relative to website dir.
          sidebarPath: require.resolve('./sidebars.js'),
          // Where to point users when they click "Edit this page"
          editUrl: fbContent({
            internal:
              'https://www.internalfb.com/intern/diffusion/FBS/browse/master/fbcode/tools/pysa/website/',
            external:
              'https://github.com/facebook/pysa/tree/main/website',
          }),
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
        enableEditor: true,
        staticDocsProject: 'pysa',
        trackingFile: 'fbcode/staticdocs/WATCHED_FILES',
        'remark-code-snippets': {
          baseDir: '../..',
        },
      },
    ],
  ],

  themeConfig: {
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    announcementBar: {
      id: 'support_ukraine',
      content:
        'Support Ukraine 🇺🇦 <a target="_blank" rel="noopener noreferrer" href="https://opensource.fb.com/support-ukraine"> Help Provide Humanitarian Aid to Ukraine</a>.',
      backgroundColor: '#20232a',
      textColor: '#fff',
      isCloseable: false,
    },
    colorMode: {
      defaultMode: 'light',
      disableSwitch: true,
    },
    navbar: {
      logo: {
        alt: 'Pysa Logo',
        src: 'img/integrated_logo_dark_bg.png'
      },
    },
    footer: {
      logo: {
        alt: 'Facebook Open Source Logo',
        src: 'img/oss_logo.png',
        href: 'https://opensource.facebook.com/',
      },
      links: [
        {
          title: 'Legal',
          // Please do not remove the privacy and terms, it's a legal requirement.
          items: [
            {
              label: 'Privacy',
              href: 'https://opensource.facebook.com/legal/privacy/',
              target: '_blank',
              rel: 'noreferrer noopener',
            },
            {
              label: 'Terms',
              href: 'https://opensource.facebook.com/legal/terms/',
              target: '_blank',
              rel: 'noreferrer noopener',
            },
          ],
        },
      ],
      copyright: `Copyright &#169; ${new Date().getFullYear()} Meta Platforms, Inc.`,
    },
    image: 'img/docusaurus.png',
  },
  customFields: {
    fbRepoName: 'fbsource',
    ossRepoPath: 'fbcode/tools/pysa',
  },
});
