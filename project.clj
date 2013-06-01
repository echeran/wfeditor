;; the following project.clj works for Leiningen 2.0.0

;; in order to upgrade from Leiningen 1.x to 2.0,
;; local/native/non-Clojar libs need to be installed into the
;; (default) Maven repo (in ~/.m2/repository) by executing the
;; maven commands on the actual jar files as indicated below, adding
;; the corresponding entries to the dependencies field of the
;; defproject, and creating a repository entry that uses the maven url
;; as the entry's key-value pair's value (see below)
;; also refer to the official Leiningen documentation on upgrading
;; from 1.x to 2.0 at https://github.com/technomancy/leiningen/wiki/Upgrading

;; based on the StackOverflow question about multi-platform build via
;; leiningen:
;; http://stackoverflow.com/questions/4688336/what-is-an-elegant-way-to-set-up-a-leiningen-project-that-requires-different-dep
(let [properties (select-keys (into {} (System/getProperties))
                              ["os.arch" "os.name"])
      platform (apply format "%s (%s)" (vals properties))
      jvm-opts (if (= "Mac OS X" (get properties "os.name"))
                ["-XstartOnFirstThread"]
                []) 
      ;; swt (case platform
      ;;       "Windows XP (x86)" '[org.eclipse/swt-win32-win32-x86 "3.5.2"]
      ;;       "Linux (x86)"      '[org.eclipse/swt-gtk-linux-x86 "3.5.2"])
      ]
  (defproject wfeditor/wfeditor "1.0.0-SNAPSHOT" 
    :dependencies [[org.clojure/clojure "1.3.0"]
                   ;; clojure-contrib 1.2.1 doesn't exist, 1.2.0 is last on 1.2x branch
                   [org.clojure/clojure-contrib "1.2.0"]
                   [local/commons-exec "1.1"]
                   [clj-http "0.6.4"]
                   [cheshire "4.0.0"]
                   [clj-ssh "0.3.1"]
                   [fs "1.1.2"]
                   [noir "1.2.1"]
                   [local/commons-lang "2.6"]
                   [local/apache-batik-css "1.6.0.v201011041432"]
                   [local/apache-batik-dom-svg "1.6.0.v201011041432"]
                   [local/apache-batik-dom "1.6.0.v201011041432"]
                   [local/apache-batik-dom-svg "1.6.0.v201011041432"] 
                   [local/apache-batik-ext-awt "1.6.0.v201011041432"]
                   [local/apache-batik-svggen "1.6.0.v201011041432"]
                   [local/apache-batik-util "1.6.0.v201011041432"]
                   [local/apache-batik-xml "1.6.0.v201011041432"] 
                   [local/eclipse-core-commands "3.6.1.v20120521-2329"] 
                   [local/eclipse-core-runtime "3.8.0.v20120521-2346"]
                   [local/eclipse-draw2d "3.8.0.201206112118"] 
                   [local/eclipse-equinox-common "3.6.100.v20120522-1841"]
                   [local/eclipse-equinox-registry "3.5.200.v20120522-1841"]
                   [local/eclipse-gmf-runtime-common-ui "1.5.0.v20120514-1615"]
                   [local/eclipse-gmf-runtime-draw2d-ui-render-awt "1.4.1.v20120514-1615"]
                   [local/eclipse-gmf-runtime-draw2d-ui-render "1.4.1.v20120514-1615"]
                   [local/eclipse-gmf-runtime-draw2d-ui "1.5.0.v20120514-1615"]
                   [local/eclipse-jface-databinding "1.6.0.v20120521-2329"]
                   [local/eclipse-jface-text "3.8.0.v20120531-0600"]
                   [local/eclipse-jface "3.8.0.v20120521-2329"]
                   [local/eclipse-osgi "3.8.0.v20120529-1548"]
                   [local/eclipse-swt-native "3.100.0.v4233d"]
                   [local/eclipse-swt "3.100.0.v4233d"]
                   [local/eclipse-ui-workbench "3.103.0.v20120530-1824"]
                   [local/eclipse-zest-cloudio "2.0.0.201207270002"]
                   [local/eclipse-zest-core "2.0.0.201207270002"] 
                   [local/eclipse-zest-dot-core "2.0.0.201207270002"]
                   [local/eclipse-zest-dot-export "2.0.0.201207270002"]
                   [local/eclipse-zest-dot-ui "2.0.0.201207270002"]
                   [local/eclipse-zest-jface "2.0.0.201207270002"]
                   [local/eclipse-zest-layouts "2.0.0.201207270002"] 
                   [local/w3c-dom-svg "1.1.0.v201011041433"]
                   ]
    ;; installed local dependencies into local maven repo (at ~/.m2 by
    ;; default)
    ;; inspired by the following SO pages, 
    ;; http://stackoverflow.com/questions/8738664/dependencies-in-maven-local-repositories-with-leiningen
    ;; http://stian.almaas.me/2012/08/adding-jars-to-local-repository-with.html
    ;; http://www.pgrs.net/2011/10/30/using-local-jars-with-leiningen/
    ;; but ultimately, I followed the link
    ;; https://gist.github.com/3062743
    ;; and used my own modifcation on that link, as follows:
    ;; to install the desired local jar file into the local maven repo ~/wfeditor/lib/commons-exec-1.1.jar, the commands are
    ;; cd ~
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=commons-exec -Dversion=1.1 -Dpackaging=jar -Dfile=wfeditor/lib/commons-exec-1.1.jar
    ;; and the file gets installed into
    ;; ~/.m2/repository/local/commons-exec/1.1, the dependency is set
    ;; in leiningen 2 as [local/commons-exec "1.1"], and the
    ;; respository is set in leiningen 2 as {"project "file:repo"}
    ;;
    ;; another example:
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=commons-lang -Dversion=2.6 -Dpackaging=jar -Dfile=wfeditor/lib/commons-lang-2.6.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=apache-batik-css -Dversion=1.6.0.v201011041432 -Dpackaging=jar -Dfile=wfeditor/lib/org.apache.batik.css_1.6.0.v201011041432.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=apache-batik-dom-svg -Dversion=1.6.0.v201011041432 -Dpackaging=jar -Dfile=wfeditor/lib/org.apache.batik.dom.svg_1.6.0.v201011041432.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=apache-batik-dom -Dversion=1.6.0.v201011041432 -Dpackaging=jar -Dfile=wfeditor/lib/org.apache.batik.dom_1.6.0.v201011041432.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=apache-batik-ext-awt -Dversion=1.6.0.v201011041432 -Dpackaging=jar -Dfile=wfeditor/lib/org.apache.batik.ext.awt_1.6.0.v201011041432.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=apache-batik-svggen -Dversion=1.6.0.v201011041432 -Dpackaging=jar -Dfile=wfeditor/lib/org.apache.batik.svggen_1.6.0.v201011041432.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=apache-batik-util -Dversion=1.6.0.v201011041432 -Dpackaging=jar -Dfile=wfeditor/lib/org.apache.batik.util_1.6.0.v201011041432.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=apache-batik-xml -Dversion=1.6.0.v201011041432 -Dpackaging=jar -Dfile=wfeditor/lib/org.apache.batik.xml_1.6.0.v201011041432.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-core-commands -Dversion=3.6.1.v20120521-2329 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.core.commands_3.6.1.v20120521-2329.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-core-runtime -Dversion=3.8.0.v20120521-2346 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.core.runtime_3.8.0.v20120521-2346.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-draw2d -Dversion=3.8.0.201206112118 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.draw2d_3.8.0.201206112118.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-equinox-common -Dversion=3.6.100.v20120522-1841 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.equinox.common_3.6.100.v20120522-1841.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-equinox-registry -Dversion=3.5.200.v20120522-1841 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.equinox.registry_3.5.200.v20120522-1841.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-gmf-runtime-common-ui -Dversion=1.5.0.v20120514-1615 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.gmf.runtime.common.ui_1.5.0.v20120514-1615.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-gmf-runtime-draw2d-ui-render-awt -Dversion=1.4.1.v20120514-1615 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.gmf.runtime.draw2d.ui.render.awt_1.4.1.v20120514-1615.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-gmf-runtime-draw2d-ui-render -Dversion=1.4.1.v20120514-1615 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.gmf.runtime.draw2d.ui.render_1.4.1.v20120514-1615.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-gmf-runtime-draw2d-ui -Dversion=1.5.0.v20120514-1615 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.gmf.runtime.draw2d.ui_1.5.0.v20120514-1615.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-jface-databinding -Dversion=1.6.0.v20120521-2329 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.jface.databinding_1.6.0.v20120521-2329.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-jface-text -Dversion=3.8.0.v20120531-0600 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.jface.text_3.8.0.v20120531-0600.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-jface -Dversion=3.8.0.v20120521-2329 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.jface_3.8.0.v20120521-2329.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-osgi -Dversion=3.8.0.v20120529-1548 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.osgi_3.8.0.v20120529-1548.jar

    ;; TODO: change this depending on the OS+chip architecture

    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-swt-native -Dversion=3.100.0.v4233d -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.swt.cocoa.macosx.x86_64_3.100.0.v4233d.jar


    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-swt -Dversion=3.100.0.v4233d -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.swt_3.100.0.v4233d.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-ui-workbench -Dversion=3.103.0.v20120530-1824 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.ui.workbench_3.103.0.v20120530-1824.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-zest-cloudio -Dversion=2.0.0.201207270002 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.zest.cloudio_2.0.0.201207270002.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-zest-core -Dversion=2.0.0.201207270002 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.zest.core_2.0.0.201207270002.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-zest-dot-core -Dversion=2.0.0.201207270002 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.zest.dot.core_2.0.0.201207270002.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-zest-dot-export -Dversion=2.0.0.201207270002 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.zest.dot.export_2.0.0.201207270002.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-zest-dot-ui -Dversion=2.0.0.201207270002 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.zest.dot.ui_2.0.0.201207270002.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-zest-jface -Dversion=2.0.0.201207270002 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.zest.jface_2.0.0.201207270002.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=eclipse-zest-layouts -Dversion=2.0.0.201207270002 -Dpackaging=jar -Dfile=wfeditor/lib/org.eclipse.zest.layouts_2.0.0.201207270002.jar
    ;; mvn install:install-file -Durl=file:repo -DgroupId=local -DartifactId=w3c-dom-svg -Dversion=1.1.0.v201011041433 -Dpackaging=jar -Dfile=wfeditor/lib/org.w3c.dom.svg_1.1.0.v201011041433.jar


    
    


    :aot [wfeditor.main]
    :main wfeditor.main
    :min-lein-version "2.0.0"

    :repositories {"project" "file:repo"}
    
    :disable-deps-clean true
    :jvm-opts ~jvm-opts
    :description "Workflow Welder: a user interface for managing the execution of complex scientific computation on various compute infrastructures"
    ;; enable a lein repl option via aliases that allows SWT to work
    ;; in repl JVM, as described in
    ;; http://stackoverflow.com/questions/14046952/using-swt-with-lein-repl-on-mac-os-x
    ;; this only works for Leiningen 2.x
    :aliases {"dumbrepl" ["trampoline" "run" "-m" "clojure.main/main"]}
    ;; need to put in an uberjar-exclusion to prevent "lein uberjar"
    ;; from putting in a .sf file in the standalone .jar that causes a
    ;; security exception when run.  This is as according to the SO info
    ;; at http://stackoverflow.com/questions/7892244/leiningen-has-problems-building-a-working-uberjar
    :uberjar-exclusions [#"ECLIPSEF.SF"
                         #"ECLIPSE_.SF"
                         #"wfeditor.*\.clj$"
                         #"project.clj"
                         #"code_jam.*.clj$"
                         #"sample.*xml$"]

    ;; including the :omit-source keyword since this may be a new
    ;; addition for Leiningen 2.x, and in a test project, the
    ;; uberjar-exclusions wasn't enough to remove the single source
    ;; file, but the omit-source key did work
    :omit-source true
    
    ))
