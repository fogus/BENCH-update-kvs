# bench-destructure

Benching Clojure map destructure RHS creation.

## Usage

To bench Clojure 1.10.2, run:

    clj -X:bench [:iterations <loop iterations>]?

To bench against a local dev version, run:

    clj -Sdeps '{:deps {org.clojure/clojure {:mvn/version "1.11.0-master-SNAPSHOT"}}}' \
	-X:bench [:iterations <loop iterations>]?

To run the dispatch check:

    JAVA_HOME=<path to jdk> clj -X:dispatch
	# e.g. 
	JAVA_HOME=/usr/local/opt/openjdk@8/ clj -X:dispatch

## License

Copyright © 2021 Fogus

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
