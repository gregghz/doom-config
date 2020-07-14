#!/usr/bin/env python3

import argparse
from pathlib import Path
import json
import os

def findBloopDir(filename):
    if filename == Path('/Users/gher33'):
        return None
    path = Path(filename)
    bloopDir = list(path.glob('.bloop'))
    if len(bloopDir) == 0:
        return findBloopDir(path.parent)
    else:
        return bloopDir[0]

def findCurrentProject(filename):
    if filename == 'nil':
        filename = os.getcwd()
    bloopDir = findBloopDir(filename)
    if not bloopDir:
        return ''
    projectFiles = list(bloopDir.glob('*.json'))
    directoryMatch = '' # fallback matching for things that live outside sources
    directoryMatchProject = ''
    for file in projectFiles:
        if os.path.basename(file) == 'bloop.settings.json':
            continue
        openFile = file.open('r')
        data = json.load(openFile)
        sources = data['project']['sources']

        inSources = any(map(lambda x: filename.startswith(x), sources))
        if inSources:
            return data['project']['name']
        elif filename.startswith(data['project']['directory']):
            if len(data['project']['directory']) > len(directoryMatch):
                directoryMatch = data['project']['directory']
                directoryMatchProject = data['project']['name']
    return directoryMatchProject

parser = argparse.ArgumentParser(description='Find the bloop project of the given file')
parser.add_argument('--file', dest='file', type=str)

args = parser.parse_args()

print(findCurrentProject(args.file), end='')
