# Build Manifest — Local GTK4 / Qt5 packages

이 문서는 로컬에서 만든 tomboy-ng GTK4/Qt5 `.deb` 패키지가 **어떤 소스 조합으로**
빌드됐는지 고정한다. KControls 수정본이 아직 업스트림에 머지되지 않았으므로,
이 리비전이 없으면 패키지를 재현할 수 없다.

빌드 일자: 2026-09-11 (`onion3`, 업스트림 v0.42c 위로 리베이스한 트리)

## Version

로컬 빌드는 업스트림 릴리스와 구분하기 위해 접미사를 붙인다.

| | 값 |
| --- | --- |
| 업스트림 버전 (`package/version`) | `0.42c` |
| 로컬 접미사 (`LOCAL_VERSION_SUFFIX`) | `onion3` (GTK4, Qt5 공통) |
| 패키지/앱 버전 | `0.42c+onion3` |

`onion3` 는 브랜치를 업스트림 v0.42c (`origin/master` `08de213`) 위로 리베이스한
뒤 두 위젯셋을 같은 KControls 리비전에서 연속 빌드한 것이다. 이전 `onion2`(GTK4) /
`onion1`(Qt5) 는 v0.42 소스 기반이며 대체되었다.

`Version_string`(`source/cli.pas:61`)이 컴파일 시점 `%TOMBOY_NG_VER`를 읽으므로
About 다이얼로그와 `--version` 출력에도 `0.42c+onion3`이 표시된다.
`package/version` 자체는 업스트림 추적 파일이므로 수정하지 않는다.

## Pinned revisions

| 저장소 | 경로 | 브랜치 | 커밋 |
| --- | --- | --- | --- |
| tomboy-ng | `/mnt/STORAGE16T/Workspace_STORAGE16T/tomboy-ng` | `gtk4-build-editor-fallback` (base `origin/master` `08de213`, v0.42c) | 문서 커밋 직전 HEAD (`git log --oneline origin/master..` 의 첫 8 개) |
| KControls | `/mnt/STORAGE16T/Workspace_STORAGE16T/KControls` (`../KControls`) | `integration-fixes` | `3770e33` (onion2 의 `74d3257` 이후 KMemo 자소 클러스터·단어 이동·UTF-16 수정과 테스트/문서가 추가됨) |

KControls 는 GTK4/Qt5 수정(PR 76/77/78 및 Phase 1~7 작업)의 합집합이며 업스트림 미머지다.
**설치되어 있지 않다.** 빌드 스크립트가 위젯셋마다 이 트리를 `/tmp` 로 복사해
격리된 Lazarus primary config path 에 등록하고 `--ws` 로 재빌드한다.
사용자 PCP(`~/.lazarus`)의 KControls 등록은 사용되지 않는다.

GTK4 와 Qt5 바이너리는 **동일한 KControls 리비전**에서 연속으로 빌드했고,
빌드 전후 HEAD 가 같음을 확인했다.

## Toolchain

| | 값 |
| --- | --- |
| FPC | 3.2.2 |
| Lazarus | `/usr/lib/lazarus/4.4` (`lazarus-src-4.4` 4.4+dfsg-4) |
| LCL GTK4 | `lcl-gtk4` 4.4+dfsg-4 (로컬 빌드 패키지, `../LCL_GTK4/deb_build/4.4`) |
| LCL Qt5 | `lcl-qt5` 4.4+dfsg-4 |

`4.4+dfsg-4` 는 `../LCL_GTK4/lazarus` 커밋 `49195ea` 의 GTK4 close-request 수정을
담은 재패키징이다. 수정 전에는 폼이 `CloseQuery` 에서 `CanClose := False` 로 숨기기만
해도 GTK4 기본 핸들러가 창을 파괴해, tomboy-ng 의 Search 창을 닫았다 다시 열면
빈 창이 뜨거나 아예 뜨지 않았다. 설치된 유닛에 수정이 들어갔는지는
`objdump -dr .../lcl/units/x86_64-linux/gtk4/gtk4widgets.o` 에서 `GTK4CLOSEQUERY` 함수
안에 `TWINCONTROL_..._HANDLEALLOCATED` 재배치가 있는지로 확인한다 (`-r` 없이는
호출 대상이 보이지 않는다).

2026-09-02 시스템 업데이트 이후 `/usr/lib/lazarus/4.4/lcl/interfaces/gtk4` 는
작업용 트리(`/mnt/STORAGE16T/Workspace_STORAGE16T/LCL_GTK4/lazarus`, 즉
`../LCL_GTK4/lazarus`)와 **완전히 동일**하다. 즉 GTK4 LCL 수정이
배포판 패키지에 반영되었으므로 더 이상 개인 워크스페이스 트리가 필요하지 않다.
Qt5 인터페이스도 `Makefile` / `cbindings/COPYING.TXT` 외에는 차이가 없다.

`build_widgetset_clean.sh` 는 설치된 Lazarus 를 우선 탐색하고,
없을 때만 워크스페이스 트리로 폴백한다. KControls 는 이 저장소 옆의
`../KControls` 에서 찾는다. 모든 작업 트리는 2026-09-02 에
`/mnt/USERS/onion/DATA_ORIGN/Workspace/` 에서
`/mnt/STORAGE16T/Workspace_STORAGE16T/` 로 옮겨졌다.

## Build

```bash
WIDGETSET=gtk4 LOCAL_VERSION_SUFFIX=onion3 ./build_widgetset_clean.sh
WIDGETSET=qt5  LOCAL_VERSION_SUFFIX=onion3 ./build_widgetset_clean.sh
```

`BUILD_MODES=GTK4` 처럼 일부 모드만 빌드해도 된다. 기대 바이너리와 스모크 테스트
대상은 선택한 모드에서 도출된다.

두 위젯셋은 `source/lib/x86_64-linux` 를 공유하므로 **반드시 순차 실행**한다.
KControls 유닛이 섞이지 않도록 빌드 루트와 PCP 는 위젯셋별로 분리된다
(`/tmp/tomboy-ng-<widgetset>-build`).

| 위젯셋 | 빌드 모드 | 바이너리 |
| --- | --- | --- |
| gtk4 | `GTK4` | `source/tomboy-ng-gtk4-dbg` |
| gtk4 | `ReleaseGTK4` | `source/tomboy-ng-gtk4` |
| qt5 | `ReleaseQT5` | `source/tomboy-ng-qt5` |

## Package

```bash
cd package
LOCAL_VERSION_SUFFIX=onion3 bash package.bash /usr/lib/lazarus/4.4 DebOnly:ReleaseGTK4
LOCAL_VERSION_SUFFIX=onion3 bash package.bash /usr/lib/lazarus/4.4 DebOnly:ReleaseQT5
```

`DebOnly:` 는 이미 빌드된 바이너리를 패키징만 한다. 재빌드하지 않으므로
위 격리 빌드의 결과가 그대로 들어간다.

| 패키지 | Depends | 비고 |
| --- | --- | --- |
| `tomboy-ng_0.42c+onion3-0_amd64GTK4.deb` | `libgtk-4-1 (>= 4.6), libnotify4 (>= 0.7), libc6 (>= 2.34)` | 현재 GTK4 패키지 (arch 태그는 업스트림 `amd64GTK3` 표기에 맞춰 `amd64GTK4`) |
| `tomboy-ng_0.42c+onion3-0_amd64Qt5.deb` | `libqt5pas1 (>= 2.15), libc6 (>= 2.34), libnotify-bin` | 현재 Qt5 패키지 |
| `tomboy-ng_0.42+onion2-0_amd64Gtk4.deb`, `tomboy-ng_0.42+onion1-0_amd64Qt5.deb` | 위와 동일 | **대체됨.** v0.42 소스와 KControls `74d3257`/`8e41d45` 기반. 결함은 없으나 `onion3` 확인 후 삭제 |

`libc6` 하한은 추측이 아니라 패키징 대상 바이너리의 최대 `GLIBC_` 심볼 버전에서
산출한다. 따라서 빌드 호스트가 달라져도 값이 따라간다.

## Known limitations

- KControls 가 업스트림 미머지 → 이 패키지는 **비공식 로컬 빌드**다.
- KControls 의 Windows / macOS IME 수정은 컴파일 검증조차 되지 않았다
  (해당 저장소 `HANDOFF.md`). 리눅스 GTK4/Qt5 패키지 범위에는 영향이 없다.
- LCL Qt5 콤보 버그 B7 / B8 / B9 는 미수정 상태로 남는다
  (`LCL_GTK4/TODO.md`).
- `dch` 가 `0.42c+onion3` 을 Debian native 버전으로 보고 경고를 낸다.
  changelog 항목에만 영향이 있고 패키지 자체는 정상이다.
