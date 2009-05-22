python << EOF
import vim
import os.path
from subprocess import Popen, PIPE

_SRC_PATH="src/main/java"
_TEST_PATH="src/test/java"
_WEBAPP_PATH="src/main/webapp"

def update_classpath_from_maven():
    print "Updating class path stuff..."
    p1 = Popen(["mvn",
                "dependency:list",
                "-DoutputAbsoluteArtifactFilename=true"], stdout=PIPE)
    libs = []
    output = p1.communicate()[0]
    for line in output.split("\n"):
        pieces = line.split(":")
        if len(pieces) == 6:
            libs.append(pieces[5])
    libs.append("target/classes")
    libs.append("target/test-classes")

    vim.command("let g:vjde_lib_path=\"%s\"" % ":".join(libs))

    if os.path.isdir(_SRC_PATH):
        vim.command("let g:vjde_src_path=\"%s\"" % _SRC_PATH)

    if os.path.isdir(_TEST_PATH):
        vim.command("let g:vjde_test_path=\"%s\"" % _SRC_PATH)

    if os.path.isdir(_WEBAPP_PATH):
        vim.command("let g:vjde_web_app=\"%s\"" % _SRC_PATH)

    vim.command("let g:vjde_java_cfu={}")

EOF
map <Leader>jup :py update_classpath_from_maven()<CR>
