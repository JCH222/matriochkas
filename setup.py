# coding: utf8

from setuptools import setup, find_packages

import matriochkas

setup(

    name='matriochkas',

    version=matriochkas.__version__,

    packages=find_packages(),

    author="Jean-Charles Hardy",

    author_email="",

    description="Flexible text parsing library",

    long_description=open('README.md').read(),

    include_package_data=True,

    url='https://github.com/JCH222/matriochkas',

    classifiers=[
        "Programming Language :: Python :: 3.6",
    ],

    license="MIT",
)
